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
    taste,
    BoiledBeans,
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
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Functor ((<&>))
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

newtype Cauldron ap m where
  Cauldron :: {recipes :: Map TypeRep (SomeBean ap m)} -> Cauldron ap m
  deriving newtype (Semigroup, Monoid)

data SomeBean ap m where
  SomeBean :: (Typeable a) => Bean ap m a -> SomeBean ap m

-- | Instructions for building a value of type @bean@, possibly requiring
-- actions in the monad @m@
data Bean ap m bean where
  Bean ::
    { 
      -- | How to build the bean itself.
      constructor :: Constructor ap m bean,
      -- | How to build the bean decorators.
      decos :: Decos ap m bean
    } ->
    Bean ap m bean

-- | A map of fully constructed beans. See 'taste'.
newtype BoiledBeans where
  BoiledBeans :: {beans :: Map TypeRep Dynamic} -> BoiledBeans

-- | A 'Bean' without decorators, having only the main constructor.
makeBean :: Constructor ap m a -> Bean ap m a
makeBean constructor = Bean {constructor, decos = mempty}

-- | A collection of 'Constructor's for the decorators of some 'Bean'.
newtype Decos ap m bean where
  Decos :: {decoCons :: Seq (Constructor ap m bean)} -> Decos ap m bean
  deriving newtype (Semigroup, Monoid)

instance IsList (Decos ap m bean) where
  type Item (Decos ap m bean) = Constructor ap m bean
  fromList decos = Decos do GHC.Exts.fromList decos
  toList (Decos {decoCons}) = GHC.Exts.toList decoCons

setConstructor :: Constructor ap m bean -> Bean ap m bean -> Bean ap m bean
setConstructor constructor (Bean {decos}) = Bean {constructor, decos}

setDecos :: Decos ap m bean -> Bean ap m bean -> Bean ap m bean
setDecos decos (Bean {constructor}) = Bean {constructor, decos}

overDecos :: (Decos ap m bean -> Decos ap m bean) -> Bean ap m bean -> Bean ap m bean
overDecos f (Bean {constructor, decos}) = Bean {constructor, decos = f decos}

addFirst :: Constructor ap m bean -> Decos ap m bean -> Decos ap m bean
addFirst con (Decos {decoCons}) = Decos do con Seq.<| decoCons

addLast :: Constructor ap m bean -> Decos ap m bean -> Decos ap m bean
addLast con (Decos {decoCons}) = Decos do decoCons Seq.|> con

fromConstructors :: 
    -- | The constructors end in 'Endo' because we are building decorators.
    [Constructor ap m bean] -> 
    Decos ap m bean
fromConstructors cons = Decos do Seq.fromList cons

-- | A way of building some @bean@ value, potentially requiring some
-- dependencies, also potentially returning some secondary beans along the
-- primary @bean@ one.
--
-- See @pack_@ and @pack@.
data Constructor ap m bean where
  Constructor ::
    (All Typeable args, All (Typeable `And` Monoid) regs) =>
    { constructor_ :: ap (Args args (m (Regs regs bean)))
    } ->
    Constructor ap m bean

data ConstructorReps where
  ConstructorReps ::
    { argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

newtype Extractor a where
  Extractor :: {runExtractor :: Map TypeRep Dynamic -> a} -> Extractor a
  deriving newtype (Functor, Applicative)

-- | The empty 'Cauldron'.
empty :: Cauldron ap m
empty = mempty

-- | Put a recipe for a 'Bean' into the 'Cauldron'.
insert ::
  forall (bean :: Type) ap m.
  (Typeable bean) =>
  Bean ap m bean ->
  Cauldron ap m ->
  Cauldron ap m
insert recipe Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
  Cauldron {recipes = Map.insert rep (SomeBean recipe) recipes}

-- | Tweak an already existing 'Bean'.
adjust ::
  forall bean ap m.
  (Typeable bean) =>
  (Bean ap m bean -> Bean ap m bean) ->
  Cauldron ap m ->
  Cauldron ap m
adjust f (Cauldron {recipes}) = do
  let rep = typeRep (Proxy @bean)
  Cauldron
    { recipes =
        Map.adjust
          do
            \(SomeBean (r :: Bean ap m a)) ->
              case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                Nothing -> error "should never happen"
                Just Refl -> SomeBean (f r)
          rep
          recipes
    }

delete ::
  forall bean ap m.
  (Typeable bean) =>
  Cauldron ap m ->
  Cauldron ap m
delete Cauldron {recipes} =
  Cauldron {recipes = Map.delete (typeRep (Proxy @bean)) recipes}

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330
constructorReps :: (Typeable component) => Constructor ap m component -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: ap (Args args (m (Regs accums component))))} =
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

constructorEdges ::
  (Typeable component) =>
  (Set TypeRep -> Set TypeRep) ->
  PlanItem ->
  Constructor ap m component ->
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

type Plan = [PlanItem]

data PlanItem
  = BareBean TypeRep
  | BeanDecorator TypeRep Integer
  | BuiltBean TypeRep
  deriving stock (Show, Eq, Ord)

-- | Build the beans using the constructors stored in the 'Cauldron'.
cook ::
  (Applicative ap, Monad m) =>
  Cauldron ap m ->
  Either BadBeans (DependencyGraph, ap (m BoiledBeans))
cook Cauldron {recipes} = do
  accumSet <- first DoubleDutyBeans do checkNoDoubleDutyBeans recipes
  () <- first MissingDependencies do checkMissingDeps (Map.keysSet accumSet) recipes
  (graph, plan) <- first DependencyCycle do checkCycles recipes
  Right
    ( DependencyGraph {graph},
      sequenceRecipes recipes <&> \recipes' -> do
        beans <- followPlan recipes' accumSet plan
        pure do BoiledBeans {beans}
    )

sequenceRecipes ::
  forall ap m.
  (Applicative ap) =>
  Map TypeRep (SomeBean ap m) ->
  ap (Map TypeRep (SomeBean I m))
sequenceRecipes = traverse sequenceSomeBean
  where
    sequenceSomeBean :: SomeBean ap m -> ap (SomeBean I m)
    sequenceSomeBean (SomeBean theBean) =
      SomeBean <$> sequenceBean theBean
    sequenceBean :: Bean ap m a -> ap (Bean I m a)
    sequenceBean Bean {constructor, decos} =
      Bean <$> sequenceConstructor constructor <*> sequenceDecos decos
    sequenceConstructor :: Constructor ap m a -> ap (Constructor I m a)
    sequenceConstructor Constructor {constructor_} =
      Constructor . I <$> constructor_
    sequenceDecos :: Decos ap m a -> ap (Decos I m a)
    sequenceDecos Decos {decoCons} = Decos <$> traverse sequenceConstructor decoCons

checkNoDoubleDutyBeans ::
  Map TypeRep (SomeBean ap m) ->
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
  Map TypeRep (SomeBean ap m) ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDeps accumSet recipes = do
  let demandedMap = Set.filter (`Map.notMember` recipes) . demanded <$> recipes
  if Data.Foldable.any (not . Set.null) demandedMap
    then Left demandedMap
    else Right ()
  where
    demanded :: SomeBean ap m -> Set TypeRep
    demanded (SomeBean Bean {constructor, decos = Decos {decoCons}}) =
      ( Set.fromList do
          let ConstructorReps {argReps = beanArgReps} = constructorReps constructor
          Set.toList beanArgReps ++ do
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps {argReps = decoArgReps} = constructorReps decoCon
            Set.toList decoArgReps
      )
        `Set.difference` accumSet

checkCycles ::
  Map TypeRep (SomeBean ap m) ->
  Either (Graph.Cycle PlanItem) (AdjacencyMap PlanItem, Plan)
checkCycles recipes = do
  let graph =
        Graph.edges
          do
            flip
              Map.foldMapWithKey
              recipes
              \beanRep
               ( SomeBean
                   ( Bean
                       { constructor = constructor :: Constructor ap m bean,
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
  case Graph.topSort graph of
    Left recipeCycle ->
      Left recipeCycle
    Right (reverse -> plan) -> Right (graph, plan)

followPlan :: Monad m =>
  Map TypeRep (SomeBean I m) ->
  Map TypeRep Dynamic ->
  Plan ->
  m (Map TypeRep Dynamic)
followPlan recipes initial plan = do
        Data.Foldable.foldlM
          do
            \super -> \case
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
          initial
          plan

data BadBeans
  = -- | Beans that work both as primary beans and as monoidal
    -- registrations are disallowed.
    DoubleDutyBeans (Set TypeRep)
  | MissingDependencies (Map TypeRep (Set TypeRep))
    -- | Dependency cycles are disallowed except for self-dependencies.
  | DependencyCycle (NonEmpty PlanItem)
  deriving stock (Show)

newtype DependencyGraph = DependencyGraph {graph :: AdjacencyMap PlanItem}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor :: Monad m =>
  Constructor I m component ->
  Map TypeRep Dynamic ->
  m (Map TypeRep Dynamic, component)
followConstructor Constructor {constructor_ = I (Args {runArgs})} super = do
  let Extractor {runExtractor} = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor super
  results <- runArgs args
  case results of
    Regs regs bean -> do
      let inserters = cfoldMap_NP (Proxy @(Typeable `And` Monoid)) makeRegInserter regs
      pure (appEndo inserters super, bean)

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

taste' :: forall a. (Typeable a) => Map TypeRep Dynamic -> Maybe a
taste' beans = do
  let rep = typeRep (Proxy @a)
  dyn <- Map.lookup rep beans
  fromDynamic @a dyn

taste :: forall a. (Typeable a) => BoiledBeans -> Maybe a
taste BoiledBeans {beans} = taste' beans

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
  forall (args :: [Type]) r curried regs bean ap m.
  ( MulticurryableF args r curried (IsFunction curried),
    All Typeable args,
    All (Typeable `And` Monoid) regs,
    Functor ap
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  (r -> m (Regs regs bean)) ->
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  ap curried ->
  Constructor ap m bean
pack f a = Constructor do go <$> a
  where
    go curried = argsN curried <&> f

packPure ::
  forall (args :: [Type]) r curried regs bean ap m.
  ( MulticurryableF args r curried (IsFunction curried),
    All Typeable args,
    All (Typeable `And` Monoid) regs,
    Functor ap,
    Applicative m
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  (r -> Regs regs bean) ->
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  ap curried ->
  Constructor ap m bean
packPure f a = Constructor do go <$> a
  where
    go curried = argsN curried <&> pure . f