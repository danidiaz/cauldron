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
    bare,
    setConstructor,
    setDecos,
    overDecos,
    Decos,
    addFirst,
    addLast,
    fromConstructors,
    Constructor,
    Regs,
    pack_,
    -- packPure_,
    pack,
    -- packPure,
    regs1,
    regs2,
    regs3,
    BeanGraph (..),
    PlanItem (..),
    exportToDot,
    BoiledBeans,
    taste,
    -- insert',
    BadBeans (..),
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Functor ((<&>))
import Data.Kind
import Data.List qualified
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
  SomeBean :: (Typeable a) => Bean m a -> SomeBean m

data Bean m bean where
  Bean ::
    { constructor :: Constructor m bean,
      decos :: Decos m bean
    } ->
    Bean m bean

newtype BoiledBeans where
  BoiledBeans :: {beans :: Map TypeRep Dynamic} -> BoiledBeans

bare :: Constructor m a -> Bean m a
bare constructor = Bean {constructor, decos = mempty}

newtype Decos m bean where
  Decos :: {decoCons :: Seq (Constructor m (Endo bean))} -> Decos m bean
  deriving newtype (Semigroup, Monoid)

instance IsList (Decos m bean) where
  type Item (Decos m bean) = Constructor m (Endo bean)
  fromList decos = Decos do GHC.Exts.fromList decos
  toList (Decos {decoCons}) = GHC.Exts.toList decoCons

setConstructor :: Constructor m bean -> Bean m bean -> Bean m bean
setConstructor constructor (Bean {decos}) = Bean {constructor, decos}

setDecos :: Decos m bean -> Bean m bean -> Bean m bean
setDecos decos (Bean {constructor}) = Bean {constructor, decos}

overDecos :: (Decos m bean -> Decos m bean) -> Bean m bean -> Bean m bean
overDecos f (Bean {constructor, decos}) = Bean {constructor, decos = f decos}

addFirst :: Constructor m (Endo bean) -> Decos m bean -> Decos m bean
addFirst con (Decos {decoCons}) = Decos do con Seq.<| decoCons

addLast :: Constructor m (Endo bean) -> Decos m bean -> Decos m bean
addLast con (Decos {decoCons}) = Decos do decoCons Seq.|> con

fromConstructors :: [Constructor m (Endo bean)] -> Decos m bean
fromConstructors cons = Decos do Seq.fromList cons

data Constructor m component where
  Constructor ::
    (All Typeable args, All (Typeable `And` Monoid) regs) =>
    { constructor_ :: m (Args args (Regs regs component))
    } ->
    Constructor m component

data ConstructorReps where
  ConstructorReps ::
    { argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

newtype Extractor a where
  Extractor :: {runExtractor :: Map TypeRep Dynamic -> a} -> Extractor a
  deriving newtype (Functor, Applicative)

empty :: Cauldron m
empty = mempty

-- | Put a recipe (constructor) into the 'Cauldron'.
insert ::
  forall (bean :: Type) m.
  (Typeable bean) =>
  Bean m bean ->
  Cauldron m ->
  Cauldron m
insert recipe Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
  Cauldron {recipes = Map.insert rep (SomeBean recipe) recipes}

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
constructorReps :: (Typeable component) => Constructor m component -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: m (Args args (Regs accums component)))} =
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
  (TypeRep -> Bool) ->
  PlanItem ->
  Constructor m component ->
  [(PlanItem, PlanItem)]
constructorEdges allowArg item (constructorReps -> ConstructorReps {argReps, regReps}) =
  -- consumers depend on their args
  ( do
      argRep <- Set.toList argReps
      guard do allowArg argRep
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

-- | Build the @bean@s using the constructors stored in the 'Cauldron'.
cook ::
  (Applicative m) =>
  Cauldron m ->
  Either BadBeans (BeanGraph, m BoiledBeans)
cook Cauldron {recipes} = do
  accumSet <- first DoubleDutyBeans do checkNoDoubleDutyBeans recipes
  () <- first MissingDependencies do checkMissingDeps (Map.keysSet accumSet) recipes
  (beanGraph, plan) <- first DependencyCycle do checkCycles recipes
  Right
    ( BeanGraph {beanGraph},
      sequenceRecipes recipes <&> \recipes' -> do
        let beans = followPlan recipes' accumSet plan
        BoiledBeans {beans}
    )

sequenceRecipes ::
  forall m.
  (Applicative m) =>
  Map TypeRep (SomeBean m) ->
  m (Map TypeRep (SomeBean I))
sequenceRecipes = traverse sequenceSomeBean
  where
    sequenceSomeBean :: SomeBean m -> m (SomeBean I)
    sequenceSomeBean (SomeBean theBean) =
      SomeBean <$> sequenceBean theBean
    sequenceBean :: Bean m a -> m (Bean I a)
    sequenceBean Bean {constructor, decos} =
      Bean <$> sequenceConstructor constructor <*> sequenceDecos decos
    sequenceConstructor :: Constructor m a -> m (Constructor I a)
    sequenceConstructor Constructor {constructor_} =
      Constructor . I <$> constructor_
    sequenceDecos :: Decos m a -> m (Decos I a)
    sequenceDecos Decos {decoCons} = Decos <$> traverse sequenceConstructor decoCons

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

checkCycles ::
  Map TypeRep (SomeBean m) ->
  Either (Graph.Cycle PlanItem) (AdjacencyMap PlanItem, Plan)
checkCycles recipes = do
  let beanGraph =
        Graph.edges
          do
            flip
              Map.foldMapWithKey
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
                      noEdgesForSelfLoops = (/=) do typeRep (Proxy @bean)
                      beanDeps = constructorEdges noEdgesForSelfLoops bareBean constructor
                      decoDeps = concatMap (uncurry do constructorEdges (const True)) decos
                      full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [builtBean]
                      innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full)
                  beanDeps ++ decoDeps ++ innerDeps
  case Graph.topSort beanGraph of
    Left recipeCycle ->
      Left recipeCycle
    Right (reverse -> plan) -> Right (beanGraph, plan)

followPlan ::
  Map TypeRep (SomeBean I) ->
  Map TypeRep Dynamic ->
  Plan ->
  Map TypeRep Dynamic
followPlan recipes initial plan = do
  let final =
        Data.List.foldl'
          do
            \super -> \case
              BareBean rep -> case fromJust do Map.lookup rep recipes of
                SomeBean (Bean {constructor}) -> do
                  let (super', bean) = followConstructor constructor final super
                      dyn = toDyn bean
                  Map.insert (dynTypeRep dyn) dyn super'
              BuiltBean _ -> super
              BeanDecorator rep index -> case fromJust do Map.lookup rep recipes of
                SomeBean (Bean {decos = Decos {decoCons}}) -> do
                  let indexStartingAt0 = fromIntegral (pred index)
                      decoCon = fromJust do Seq.lookup indexStartingAt0 decoCons
                      (super', bean) = followDecorator decoCon final super
                      dyn = toDyn bean
                  Map.insert (dynTypeRep dyn) dyn super'
          initial
          plan
  final

data BadBeans
  = -- | Weans that work both as primary beans and as monoidal
    -- registrations are disallowed.
    DoubleDutyBeans (Set TypeRep)
  | MissingDependencies (Map TypeRep (Set TypeRep))
  | DependencyCycle (NonEmpty PlanItem)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap PlanItem}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor ::
  Constructor I component ->
  Map TypeRep Dynamic ->
  Map TypeRep Dynamic ->
  (Map TypeRep Dynamic, component)
followConstructor Constructor {constructor_ = I (Args {runArgs})} final super = do
  let Extractor {runExtractor} = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor final
  case runArgs args of
    Regs regs bean -> do
      let inserters = cfoldMap_NP (Proxy @(Typeable `And` Monoid)) makeRegInserter regs
      (appEndo inserters super, bean)

followDecorator ::
  forall component.
  (Typeable component) =>
  Constructor I (Endo component) ->
  Map TypeRep Dynamic ->
  Map TypeRep Dynamic ->
  (Map TypeRep Dynamic, component)
followDecorator decoCon final super = do
  let (super', Endo deco) = followConstructor decoCon final super
      baseDyn = fromJust do Map.lookup (typeRep (Proxy @component)) super'
      base = fromJust do fromDynamic baseDyn
  (super', deco base)

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

exportToDot :: FilePath -> BeanGraph -> IO ()
exportToDot filepath BeanGraph {beanGraph} = do
  let prettyRep =
        let p rep = Data.Text.pack do tyConName do typeRepTyCon rep
         in \case
              BareBean rep -> p rep <> Data.Text.pack "#0"
              BeanDecorator rep index -> p rep <> Data.Text.pack ("#" ++ show index)
              BuiltBean rep -> p rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          beanGraph
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
    All (Typeable `And` Monoid) regs,
    Functor m
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  (r -> Regs regs bean) ->
  -- | Constructor
  m curried ->
  Constructor m bean
pack f m = Constructor do go <$> m
  where
    go curried = argsN curried <&> f

pack_ ::
  forall (args :: [Type]) bean curried m.
  ( MulticurryableF args bean curried (IsFunction curried),
    All Typeable args,
    Functor m
  ) =>
  -- | Constructor
  m curried ->
  Constructor m bean
pack_ m = Constructor do go <$> m
  where
    go curried = argsN curried <&> Regs Nil
