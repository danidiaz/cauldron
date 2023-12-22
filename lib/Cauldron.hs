{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A library for performing dependency injection.
module Cauldron
  ( Cauldron,
    empty,
    insert,
    adjust,
    delete,
    cook,
    Bean (..),
    makeBean,
    setConstructor,
    setDecos,
    overDecos,
    Decos,
    addFirst,
    addLast,
    fromConstructors,
    Constructor,
    pack,
    packPure,
    packPure0,
    Regs,
    regs0,
    regs1,
    regs2,
    regs3,
    DependencyGraph (..),
    PlanItem (..),
    exportToDot,
    -- insert',
    BadBeans (..),
    BoiledBeans,
    taste,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap, vertexSet)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..))
import Data.SOP (All, And, K (..))
import Data.SOP.NP
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exts (IsList (..))
import Multicurryable
import Type.Reflection qualified

newtype Cauldron m where
  Cauldron :: {recipes :: Map TypeRep (SomeBean m)} -> Cauldron m
  deriving newtype (Semigroup, Monoid)

data SomeBean m where
  SomeBean :: (Typeable bean) => Bean m bean -> SomeBean m

-- | Instructions for building a value of type @bean@, possibly requiring
-- actions in the monad @m@
data Bean m bean where
  Bean ::
    { 
      -- | How to build the bean itself.
      constructor :: Constructor m bean,
      -- | How to build the bean decorators.
      decos :: Decos m bean
    } ->
    Bean m bean

-- | A 'Bean' without decorators, having only the main constructor.
makeBean :: Constructor m a -> Bean m a
makeBean constructor = Bean {constructor, decos = mempty}

-- | A collection of 'Constructor's for the decorators of some 'Bean'.
newtype Decos m bean where
  Decos :: {decoCons :: Seq (Constructor m bean)} -> Decos m bean
  deriving newtype (Semigroup, Monoid)

instance IsList (Decos m bean) where
  type Item (Decos m bean) = Constructor m bean
  fromList decos = Decos do GHC.Exts.fromList decos
  toList (Decos {decoCons}) = GHC.Exts.toList decoCons

setConstructor :: Constructor m bean -> Bean m bean -> Bean m bean
setConstructor constructor (Bean {decos}) = Bean {constructor, decos}

setDecos :: Decos m bean -> Bean m bean -> Bean m bean
setDecos decos (Bean {constructor}) = Bean {constructor, decos}

overDecos :: (Decos m bean -> Decos m bean) -> Bean m bean -> Bean m bean
overDecos f (Bean {constructor, decos}) = Bean {constructor, decos = f decos}

addFirst :: Constructor m bean -> Decos m bean -> Decos m bean
addFirst con (Decos {decoCons}) = Decos do con Seq.<| decoCons

addLast :: Constructor m bean -> Decos m bean -> Decos m bean
addLast con (Decos {decoCons}) = Decos do decoCons Seq.|> con

fromConstructors :: 
    -- | The constructors end in 'Endo' because we are building decorators.
    [Constructor m bean] -> 
    Decos m bean
fromConstructors cons = Decos do Seq.fromList cons

-- | A way of building some @bean@ value, potentially requiring some
-- dependencies, also potentially returning some secondary beans along the
-- primary @bean@ one.
--
-- See @pack_@ and @pack@.
data Constructor m bean where
  Constructor ::
    (All Typeable args, All (Typeable `And` Monoid) regs) =>
    { constructor_ :: Args args (m (Regs regs bean))
    } ->
    Constructor m bean

data ConstructorReps where
  ConstructorReps ::
    { argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

-- | The empty 'Cauldron'.
empty :: Cauldron m
empty = mempty

-- | Put a recipe for a 'Bean' into the 'Cauldron'.
insert ::
  forall (bean :: Type) m.
  (Typeable bean) =>
  Bean m bean ->
  Cauldron m ->
  Cauldron m
insert recipe Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
  Cauldron {recipes = Map.insert rep (SomeBean recipe) recipes}

-- | Tweak an already existing 'Bean'.
adjust ::
  forall bean m.
  (Typeable bean) =>
  (Bean m bean -> Bean m bean) ->
  Cauldron m ->
  Cauldron m
adjust f (Cauldron {recipes}) = do
  let rep = typeRep (Proxy @bean)
  Cauldron
    { recipes =
        Map.adjust
          do
            \(SomeBean (r :: Bean m a)) ->
              case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                Nothing -> error "should never happen"
                Just Refl -> SomeBean (f r)
          rep
          recipes
    }

delete ::
  forall bean m.
  (Typeable bean) =>
  Cauldron m ->
  Cauldron m
delete Cauldron {recipes} =
  Cauldron {recipes = Map.delete (typeRep (Proxy @bean)) recipes}

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330
constructorReps :: (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: Args args (m (Regs accums bean)))} =
  ConstructorReps
    { argReps = Set.fromList do
        collapse_NP do
          cpure_NP @_ @args
            do Proxy @Typeable
            typeRepHelper,
      regReps = Map.fromList do
        collapse_NP do
          cpure_NP @_ @accums
            do Proxy @(Typeable `And` Monoid)
            typeRepHelper'
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K (typeRep (Proxy @a))
    typeRepHelper' :: forall a. ((Typeable `And` Monoid) a) => K (TypeRep, Dynamic) a
    typeRepHelper' = K (typeRep (Proxy @a), toDyn @a mempty)

type Plan = [PlanItem]

data PlanItem
  = BareBean TypeRep
  | BeanDecorator TypeRep Integer
  | BuiltBean TypeRep
  deriving stock (Show, Eq, Ord)

newtype BoiledBeans where
  BoiledBeans :: {beans :: Map TypeRep Dynamic} -> BoiledBeans

-- | Build the beans using the constructors stored in the 'Cauldron'.
cook :: forall m.
  (Monad m) =>
  Cauldron m ->
  Either BadBeans (DependencyGraph, m BoiledBeans)
cook Cauldron {recipes} = do
  accumSet <- first DoubleDutyBeans do checkNoDoubleDutyBeans recipes
  () <- first MissingDependencies do checkMissingDeps (Map.keysSet accumSet) recipes
  let graph = buildDepGraph recipes
  plan <- case Graph.topSort graph of
    Left recipeCycle ->
      Left do DependencyCycle recipeCycle
    Right (reverse -> plan) -> Right plan
  Right
    ( DependencyGraph {graph},
      do
        beans <- followPlan recipes accumSet plan
        pure do BoiledBeans {beans}
    )

checkNoDoubleDutyBeans ::
  Map TypeRep (SomeBean m) ->
  Either (Set TypeRep) (Map TypeRep Dynamic)
checkNoDoubleDutyBeans recipes = do
  let common = Set.intersection (Map.keysSet accumSet) (Map.keysSet recipes)
  if not (Set.null common)
    then Left common
    else Right accumSet
  where
    accumSet = Map.fromList do
      recipe <- Data.Foldable.toList recipes
      case recipe of
        (SomeBean Bean {constructor, decos = Decos {decoCons}}) -> do
          let ConstructorReps {regReps = beanAccums} = constructorReps constructor
          Map.toList beanAccums ++ do
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps {regReps = decoAccums} = constructorReps decoCon
            Map.toList decoAccums

checkMissingDeps ::
  Set TypeRep ->
  Map TypeRep (SomeBean m) ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDeps accumSet recipes = do
  let demandedMap = Set.filter (`Map.notMember` recipes) . demanded <$> recipes
  if Data.Foldable.any (not . Set.null) demandedMap
    then Left demandedMap
    else Right ()
  where
    demanded :: SomeBean m -> Set TypeRep
    demanded (SomeBean Bean {constructor, decos = Decos {decoCons}}) =
      ( Set.fromList do
          let ConstructorReps {argReps = beanArgReps} = constructorReps constructor
          Set.toList beanArgReps ++ do
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps {argReps = decoArgReps} = constructorReps decoCon
            Set.toList decoArgReps
      )
        `Set.difference` accumSet

buildDepGraph :: Map TypeRep (SomeBean m) -> AdjacencyMap PlanItem
buildDepGraph recipes = Graph.edges 
  do
    (flip Map.foldMapWithKey)
      recipes
      \beanRep
        ( SomeBean
            ( Bean
                { constructor = constructor :: Constructor m bean,
                  decos = Decos {decoCons}
                }
              )
          ) -> do
          let bareBean = BareBean beanRep
              builtBean = BuiltBean beanRep
              decos = do
                (decoIndex, decoCon) <- zip [1 :: Integer ..] (Data.Foldable.toList decoCons)
                [(BeanDecorator beanRep decoIndex, decoCon)]
              beanDeps = constructorEdges id bareBean constructor
              -- We remove dependencies on bean itself from the decos. We already depend on the
              -- previous deco.
              decoDeps = concatMap (uncurry (constructorEdges (Set.delete beanRep))) decos
              full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [builtBean]
              innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full)
          beanDeps ++ decoDeps ++ innerDeps

constructorEdges ::
  (Typeable bean) =>
  (Set TypeRep -> Set TypeRep) ->
  PlanItem ->
  Constructor m bean ->
  [(PlanItem, PlanItem)]
constructorEdges tweakArgs item (constructorReps -> ConstructorReps {argReps = tweakArgs -> argReps, regReps}) =
  -- consumers depend on their args
  ( do
      argRep <- Set.toList argReps
      let argItem = BuiltBean argRep
      [(item, argItem)]
  )
    ++
    -- regs depend on their producers
    ( do
        (regRep, _) <- Map.toList regReps
        let repItem = BuiltBean regRep
        [(repItem, item)]
    )

followPlan :: Monad m =>
  Map TypeRep (SomeBean m) ->
  Map TypeRep Dynamic ->
  Plan ->
  m (Map TypeRep Dynamic)
followPlan recipes initial plan = 
        Data.Foldable.foldlM
          do followPlanStep recipes
          initial
          plan

followPlanStep :: Monad m =>
 Map TypeRep (SomeBean m) ->
 Map TypeRep Dynamic -> 
 PlanItem -> 
 m (Map TypeRep Dynamic)
followPlanStep recipes super = \case
  BareBean rep -> case fromJust do Map.lookup rep recipes of
    SomeBean (Bean {constructor}) -> do
      (super', bean) <- followConstructor constructor super
      let dyn = toDyn bean
      pure do Map.insert (dynTypeRep dyn) dyn super'
  BuiltBean _ -> pure super
  BeanDecorator rep index -> case fromJust do Map.lookup rep recipes of
    SomeBean (Bean {decos = Decos {decoCons}}) -> do
      let indexStartingAt0 = fromIntegral (pred index)
      let decoCon = fromJust do Seq.lookup indexStartingAt0 decoCons
      (super', bean) <- followConstructor decoCon super
      let dyn = toDyn bean
      pure do Map.insert (dynTypeRep dyn) dyn super'

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor :: Monad m =>
  Constructor m bean ->
  Map TypeRep Dynamic ->
  m (Map TypeRep Dynamic, bean)
followConstructor Constructor {constructor_ = Args {runArgs}} super = do
  let Extractor {runExtractor} = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor super
  results <- runArgs args
  case results of
    Regs regs bean -> do
      let inserters = cfoldMap_NP (Proxy @(Typeable `And` Monoid)) makeRegInserter regs
      pure (appEndo inserters super, bean)

newtype Extractor a where
  Extractor :: {runExtractor :: Map TypeRep Dynamic -> a} -> Extractor a
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let runExtractor dyns =
        fromJust do taste' @a dyns
   in Extractor {runExtractor}

makeRegInserter :: forall a. ((Typeable `And` Monoid) a) => I a -> Endo (Map TypeRep Dynamic)
makeRegInserter (I a) =
  let appEndo dynMap = do
        let reg = fromJust do taste' @a dynMap
            dyn = toDyn (reg <> a)
        Map.insert (dynTypeRep dyn) dyn dynMap
   in Endo {appEndo}


-- restrict :: forall bean m . Typeable bean => Cauldron m -> Cauldron m
-- restrict Cauldron {recipes} = do
--   let graph = buildDepGraph recipes
--       restrictedGraph = restrict' graph (BuiltBean (typeRep (Proxy @bean))) 
--       vertices = do
--         Set.map 
--           \case BareBean r -> r
--                 BeanDecorator r _ -> r
--                 BuiltBean r -> r
--           do vertexSet restrictedGraph
--   Cauldron { recipes = recipes `Map.restrictKeys` vertices }
-- 
-- restrict' :: Ord a => AdjacencyMap a -> a -> AdjacencyMap a
-- restrict' g v = do
--   let relevantSet = Set.fromList do Graph.reachable g v
--   let relevant = (`Set.member` relevantSet)
--   Graph.induce relevant g 

taste :: forall a. (Typeable a) => BoiledBeans -> Maybe a
taste BoiledBeans {beans} = taste' beans

taste' :: forall a. (Typeable a) => Map TypeRep Dynamic -> Maybe a
taste' beans = do
  let rep = typeRep (Proxy @a)
  dyn <- Map.lookup rep beans
  fromDynamic @a dyn
data BadBeans
  = 
    MissingDependencies (Map TypeRep (Set TypeRep))
    -- | Beans that work both as primary beans and as monoidal
    -- registrations are disallowed.
  | DoubleDutyBeans (Set TypeRep)
    -- | Dependency cycles are disallowed except for self-dependencies.
  | DependencyCycle (NonEmpty PlanItem)
  deriving stock (Show)

newtype DependencyGraph = DependencyGraph {graph :: AdjacencyMap PlanItem}

exportToDot :: FilePath -> DependencyGraph -> IO ()
exportToDot filepath DependencyGraph {graph} = do
  let prettyRep =
        let p rep = Data.Text.pack do tyConName do typeRepTyCon rep
         in \case
              BareBean rep -> p rep <> Data.Text.pack "#0"
              BeanDecorator rep index -> p rep <> Data.Text.pack ("#" ++ show index)
              BuiltBean rep -> p rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          graph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)


newtype Args args r = Args {runArgs :: NP I args -> r}
  deriving newtype (Functor, Applicative, Monad)

argsN ::
  forall (args :: [Type]) r curried.
  (MulticurryableF args r curried (IsFunction curried)) =>
  curried ->
  Args args r
argsN = Args . multiuncurry

-- | Auxiliary type which contains a primary bean along with zero or more
-- secondary monoidal beans.
data Regs (regs :: [Type]) bean = Regs (NP I regs) bean
  deriving (Functor)


regs0 :: bean -> Regs '[] bean
regs0 bean = Regs Nil bean

regs1 :: reg1 -> bean -> Regs '[reg1] bean
regs1 reg1 bean = Regs (I reg1 :* Nil) bean

regs2 :: reg1 -> reg2 -> bean -> Regs '[reg1, reg2] bean
regs2 reg1 reg2 bean = Regs (I reg1 :* I reg2 :* Nil) bean

regs3 :: reg1 -> reg2 -> reg3 -> bean -> Regs '[reg1, reg2, reg3] bean
regs3 reg1 reg2 reg3 bean = Regs (I reg1 :* I reg2 :* I reg3 :* Nil) bean

-- | To be used only for constructors which return monoidal secondary beans
-- along with the primary bean.
pack ::
  forall (args :: [Type]) r curried regs bean m.
  ( MulticurryableF args r curried (IsFunction curried),
    All Typeable args,
    All (Typeable `And` Monoid) regs
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  (r -> m (Regs regs bean)) ->
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  curried ->
  Constructor m bean
pack f curried = Constructor do f <$> do argsN curried 

packPure ::
  forall (args :: [Type]) r curried regs bean m.
  ( MulticurryableF args r curried (IsFunction curried),
    All Typeable args,
    All (Typeable `And` Monoid) regs,
    Applicative m
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  (r -> Regs regs bean) ->
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  curried ->
  Constructor m bean
packPure f curried = Constructor do pure . f <$> do argsN curried

packPure0 ::
  forall (args :: [Type]) bean curried m.
  ( MulticurryableF args bean curried (IsFunction curried),
    All Typeable args,
    Applicative m
  ) =>
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  curried ->
  Constructor m bean
packPure0 = packPure regs0
