{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | This is a library for performing dependency injection. 
--
-- >>> :{
-- data A = A deriving Show
-- data B = B deriving Show
-- makeA :: A ; makeA = A
-- makeB :: A -> B ; makeB = \_ -> B
-- makeB makeA
-- :}
-- B
--
-- into the more succint
--
-- let c = emptyCauldron

module Cauldron
  ( -- * Filling the cauldron
    Cauldron,
    emptyCauldron,
    insert,
    adjust,
    delete,
    hoistCauldron,

    -- * Beans
    Bean (..),
    makeBean,
    setConstructor,
    setDecos,
    overDecos,
    hoistBean,

    -- ** Decorators
    Decos,
    emptyDecos,
    fromConstructors,
    addOuter,
    addInner,
    hoistDecos,

    -- ** Constructors
    Constructor,
    hoistConstructor,
    pack,
    pack0,
    pack1,
    pack2,
    pack3,
    Packer (..),
    value,
    effect,

    -- *** Dealing with registrations
    -- $registrations
    valueWith,
    effectWith,
    Regs,
    regs0,
    regs1,
    regs2,
    regs3,

    -- * Cooking the beans
    cook,
    cookNonEmpty,
    cookTree,

    -- ** How loopy can we get?
    Fire,
    forbidDepCycles,
    allowSelfDeps,

    -- ** Tasting the results
    BoiledBeans,
    taste,
    BadBeans (..),
    PathToCauldron,

    -- ** Drawing deps
    DependencyGraph,
    BeanConstructionStep (..),
    removeSecondaryBeans,
    removeDecos,
    collapsePrimaryBeans,
    exportToDot,
    toAdjacencyMap,

    -- * The Managed monad for handling resources
    Managed,
    managed,
    with,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
-- import Control.Monad.Fix

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Functor (($>), (<&>))
import Data.Functor.Compose
import Data.Functor.Contravariant
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
import Data.Tree
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exts (IsList (..))
import GHC.IO.Unsafe
import Multicurryable
import Type.Reflection qualified

-- | A map of 'Bean' recipes indexed by the type of the bean.
--
-- >>> 3 :: Int
-- 4
newtype Cauldron m where
  Cauldron :: {recipes :: Map TypeRep (SomeBean m)} -> Cauldron m

-- | Union of two 'Cauldron's, right-biased: prefers values from the /right/ cauldron when
-- both contain the same bean. (Note that 'Data.Map.Map' is left-biased.)
instance Semigroup (Cauldron m) where
  Cauldron {recipes = r1 } <> Cauldron {recipes = r2} = Cauldron do Map.unionWith (flip const) r1 r2

instance Monoid (Cauldron m) where
  mempty = Cauldron do Map.empty

emptyCauldron :: Cauldron m
emptyCauldron = mempty

-- | Change the monad used by the beans in the 'Cauldron'.
hoistCauldron :: (forall x. m x -> n x) -> Cauldron m -> Cauldron n
hoistCauldron f (Cauldron {recipes}) = Cauldron {recipes = hoistSomeBean f <$> recipes}

data SomeBean m where
  SomeBean :: (Typeable bean) => Bean m bean -> SomeBean m

hoistSomeBean :: (forall x. m x -> n x) -> SomeBean m -> SomeBean n
hoistSomeBean f (SomeBean bean) = SomeBean do hoistBean f bean

-- | A bean recipe, to be inserted into a 'Cauldron'.
data Bean m bean where
  Bean ::
    { -- | How to build the bean itself.
      constructor :: Constructor m bean,
      -- | How to build the decorators that wrap the bean. There might be no decorators.
      decos :: Decos m bean
    } ->
    Bean m bean

-- | Change the monad used by the bean\'s 'Constructor' and its 'Decos'.
hoistBean :: (forall x. m x -> n x) -> Bean m bean -> Bean n bean
hoistBean f (Bean {constructor, decos}) =
  Bean
    { constructor = hoistConstructor f constructor,
      decos = hoistDecos f decos
    }

-- | A 'Bean' without decorators, having only the main constructor.
makeBean :: Constructor m a -> Bean m a
makeBean constructor = Bean {constructor, decos = mempty}

-- | A list of 'Constructor's for the decorators of some 'Bean'.
--
-- 'Constructor's for a decorator will have the @bean@ itself among their
-- arguments. That's the way each 'Constructor' gets hold of it, in order to
-- return the decorated version.
--
-- That @bean@ argument will be either the \"bare\" undecorated
-- bean (for the first decorator) or the result of applying the previous
-- decorator in the list.
--
-- Decorators can have other dependencies besides the @bean@.
newtype Decos m bean where
  Decos :: {decoCons :: Seq (Constructor m bean)} -> Decos m bean
  deriving newtype (Semigroup, Monoid)

instance IsList (Decos m bean) where
  type Item (Decos m bean) = Constructor m bean
  fromList decos = Decos do GHC.Exts.fromList decos
  toList (Decos {decoCons}) = GHC.Exts.toList decoCons

-- | Empty list of decorators.
emptyDecos :: Decos m bean
emptyDecos = mempty

-- | Change the monad used by the decorators.
hoistDecos :: (forall x. m x -> n x) -> Decos m bean -> Decos n bean
hoistDecos f (Decos {decoCons}) = Decos {decoCons = hoistConstructor f <$> decoCons}

setConstructor :: Constructor m bean -> Bean m bean -> Bean m bean
setConstructor constructor (Bean {decos}) = Bean {constructor, decos}

setDecos :: Decos m bean -> Bean m bean -> Bean m bean
setDecos decos (Bean {constructor}) = Bean {constructor, decos}

overDecos :: (Decos m bean -> Decos m bean) -> Bean m bean -> Bean m bean
overDecos f (Bean {constructor, decos}) = Bean {constructor, decos = f decos}

-- | Add a new decorator that modifies the bean /after/ all existing decorators.
--
-- This means the behaviours it adds to the bean\'s methods will be applied
-- /first/ when entering the method.
addOuter :: Constructor m bean -> Decos m bean -> Decos m bean
addOuter con (Decos {decoCons}) = Decos do decoCons Seq.|> con

-- | Add a new decorator that modifies the bean /before/ all existing
-- decorators.
--
-- This means the behaviours it adds to the bean\'s methods will be applied
-- /last/, just before entering the base bean's method.
--
-- Usually 'addOuter' is preferrable.
addInner :: Constructor m bean -> Decos m bean -> Decos m bean
addInner con (Decos {decoCons}) = Decos do con Seq.<| decoCons

-- | Build the decorators from a list of 'Constructor's, first innermost,
-- last outermost.
fromConstructors ::
  [Constructor m bean] ->
  Decos m bean
fromConstructors cons = Decos do Seq.fromList cons

-- | A way of building some @bean@ value, potentially requiring some
-- dependencies, potentially returning some secondary beans (\"registrations\")
-- along the primary @bean@ result, and also potentially requiring some
-- initialization effect in a monad @m@.
--
-- A typical initialization monad will be 'IO', used for example to create
-- mutable references that the bean will use internally. Sometimes the a
-- constructor will allocate resources with bracket-like operations, and in that
-- case a monad like 'Managed' may be needed instead.
data Constructor m bean where
  Constructor ::
    (All Typeable args, All (Typeable `And` Monoid) regs) =>
    { constructor_ :: Args args (m (Regs regs bean))
    } ->
    Constructor m bean

data ConstructorReps where
  ConstructorReps ::
    { beanRep :: TypeRep,
      argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

-- | Change the monad in which the 'Constructor'\'s effects take place.
hoistConstructor :: (forall x. m x -> n x) -> Constructor m bean -> Constructor n bean
hoistConstructor f (Constructor {constructor_}) = Constructor do fmap f constructor_

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

-- | Tweak an already existing 'Bean' recipe.
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

-- | Strategy for dealing with dependency cycles.
--
-- Terrible uninformative name caused by a metaphor stretched too far.
data Fire m = Fire
  { shouldOmitDependency :: (BeanConstructionStep, BeanConstructionStep) -> Bool,
    followPlanCauldron ::
      Cauldron m ->
      BoiledBeans ->
      Plan ->
      m BoiledBeans
  }

removeBeanFromArgs :: ConstructorReps -> ConstructorReps
removeBeanFromArgs ConstructorReps {argReps, regReps, beanRep} =
  ConstructorReps {argReps = Set.delete beanRep argReps, regReps, beanRep}

-- | Allow /direct/ self-dependencies.
--
-- A bean constructor might depend on itself. This can be useful for having
-- decorated self-invocations, because the version of the bean received as
-- argument comes \"from the future\" and is already decorated. (__BEWARE__:
-- Pattern-matching too eagerly on this \"bean from the future\" during
-- construction will cause infinite loops.)
--
-- Note that a 'MonadFix' instance is required of the initialization monad.
allowSelfDeps :: (MonadFix m) => Fire m
allowSelfDeps =
  Fire
    { shouldOmitDependency = \case
        (BarePrimaryBean bean, PrimaryBean anotherBean) | bean == anotherBean -> True
        _ -> False,
      followPlanCauldron = \cauldron initial plan ->
        mfix do
          \final ->
            Data.Foldable.foldlM
              do followPlanStep cauldron final
              initial
              plan
    }

-- | Forbid any kind of cyclic dependencies between beans.
forbidDepCycles :: (Monad m) => Fire m
forbidDepCycles =
  Fire
    { shouldOmitDependency = \_ -> False,
      followPlanCauldron = \cauldron initial plan ->
        Data.Foldable.foldlM
          do followPlanStep cauldron BoiledBeans {beans = Map.empty}
          initial
          plan
    }

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330

-- | This function DOESN'T return the bean rep itself in the argreps.
constructorReps :: (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: Args args (m (Regs accums bean)))} =
  ConstructorReps
    { beanRep,
      argReps =
        do
          Set.fromList do
            collapse_NP do
              cpure_NP @_ @args
                do Proxy @Typeable
                typeRepHelper,
      regReps =
        Map.fromList do
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
    beanRep = typeRep (Proxy @bean)

type Plan = [BeanConstructionStep]

-- | A step in building a bean value.
data BeanConstructionStep
  = -- | Undecorated bean.
    BarePrimaryBean TypeRep
  | -- | Apply a decorator. Comes after the 'BarePrimaryBean' and any 'PrimaryBeanDeco's wrapped by the current decorator.
    PrimaryBeanDeco TypeRep Int
  | -- | Final, fully decorated version of a bean. If there are no decorators, comes directly after 'BarePrimaryBean'.
    PrimaryBean TypeRep
  | -- | Beans that are secondary registrations of a 'Constructor' and which are aggregated monadically.
    SecondaryBean TypeRep
  deriving stock (Show, Eq, Ord)

-- | Can't do a lot with them other than to 'taste' them.
newtype BoiledBeans where
  BoiledBeans :: {beans :: Map TypeRep Dynamic} -> BoiledBeans

-- | Build the beans using the recipes stored in the 'Cauldron'.
cook ::
  forall m.
  (Monad m) =>
  Fire m ->
  Cauldron m ->
  Either BadBeans (DependencyGraph, m BoiledBeans)
cook fire cauldron = do
  let result = cookTree (Node (fire, cauldron) [])
  result <&> \(tg, m) -> (rootLabel tg, rootLabel <$> m)

-- | Cook a list of 'Cauldron's.
--
-- 'Cauldron's later in the list can see the beans in all previous 'Cauldron's,
-- but not vice versa.
--
-- Beans in a 'Cauldron' have priority over the same beans in previous 'Cauldron's.
cookNonEmpty ::
  forall m.
  (Monad m) =>
  NonEmpty (Fire m, Cauldron m) ->
  Either BadBeans (NonEmpty DependencyGraph, m (NonEmpty BoiledBeans))
cookNonEmpty nonemptyCauldronList = do
  let result = cookTree (nonEmptyToTree nonemptyCauldronList)
  result <&> \(ng, m) -> (unsafeTreeToNonEmpty ng, unsafeTreeToNonEmpty <$> m)

-- | Cook a hierarchy of 'Cauldron's.
--
-- 'Cauldron's down in the branches can see the beans of their ancestor
-- 'Cauldron's, but not vice versa.
--
-- Beans in a 'Cauldron' have priority over the same beans in ancestor 'Cauldron's.
cookTree ::
  forall m.
  (Monad m) =>
  Tree (Fire m, Cauldron m) ->
  Either BadBeans (Tree DependencyGraph, m (Tree BoiledBeans))
cookTree (treecipes) = do
  accumMap <- first DoubleDutyBeans do checkNoDoubleDutyBeans (snd <$> treecipes)
  () <- first (uncurry MissingDependencies) do checkMissingDeps (Map.keysSet accumMap) (snd <$> treecipes)
  treeplan <- first DependencyCycle do buildPlans (Map.keysSet accumMap) treecipes
  Right
    ( treeplan <&> \(graph, _) -> DependencyGraph {graph},
      followPlan (BoiledBeans accumMap) (snd <$> treeplan)
    )

checkNoDoubleDutyBeans ::
  Tree (Cauldron m) ->
  Either (Set TypeRep) (Map TypeRep Dynamic)
checkNoDoubleDutyBeans treecipes = do
  let (accumMap, beanSet) = cauldronTreeRegs treecipes
  let common = Set.intersection (Map.keysSet accumMap) beanSet
  if not (Set.null common)
    then Left common
    else Right accumMap

-- | Will always be @[]@ when using 'cook'; identifies a 'Cauldron' in a hierarchy of 'Cauldron's when
-- using 'cookNonEmpty' or 'cookTree'.
type PathToCauldron = [Int]

cauldronTreeRegs :: Tree (Cauldron m) -> (Map TypeRep Dynamic, Set TypeRep)
cauldronTreeRegs = foldMap cauldronRegs

cauldronRegs :: Cauldron m -> (Map TypeRep Dynamic, Set TypeRep)
cauldronRegs Cauldron {recipes} =
  Map.foldMapWithKey
    do \rep recipe -> (recipeRegs recipe, Set.singleton rep)
    recipes

-- | Returns the accumulators, not the main bean
recipeRegs :: SomeBean m -> Map TypeRep Dynamic
recipeRegs (SomeBean (Bean {constructor, decos = Decos {decoCons}})) = do
  let extractRegReps = (.regReps) . constructorReps
  extractRegReps constructor
    <> foldMap extractRegReps decoCons

checkMissingDeps ::
  -- | accums
  Set TypeRep ->
  Tree (Cauldron m) ->
  Either (PathToCauldron, Map TypeRep (Set TypeRep)) ()
checkMissingDeps accums treecipes = do
  let decoratedTreecipes = decorate ([], Map.empty, treecipes)
      missing = (\(key, available, requested) -> first (key,) do checkMissingDepsCauldron accums (Map.keysSet available) requested) <$> decoratedTreecipes
  sequence_ missing
  where
    decorate ::
      (PathToCauldron, Map TypeRep PathToCauldron, Tree (Cauldron m)) ->
      Tree (PathToCauldron, Map TypeRep PathToCauldron, Cauldron m)
    decorate = unfoldTree
      do
        \(key, acc, Node (current@Cauldron {recipes}) rest) ->
          let -- current level has priority
              newAcc = (recipes $> key) `Map.union` acc
              newSeeds = do
                (i, z) <- zip [0 ..] rest
                let newKey = key ++ [i]
                [(newKey, newAcc, z)]
           in ((key, newAcc, current), newSeeds)

checkMissingDepsCauldron ::
  -- | accums
  Set TypeRep ->
  -- | available at this level
  Set TypeRep ->
  Cauldron m ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDepsCauldron accums available Cauldron {recipes} = do
  let demandedMap = Set.filter (`Set.notMember` available) . demanded <$> recipes
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
        `Set.difference` accums

buildPlans :: Set TypeRep -> Tree (Fire m, Cauldron m) -> Either (NonEmpty BeanConstructionStep) (Tree (AdjacencyMap BeanConstructionStep, (Plan, Fire m, Cauldron m)))
buildPlans secondary = traverse \(fire@Fire {shouldOmitDependency}, cauldron) -> do
  let deps = filter (not . shouldOmitDependency) do buildDepsCauldron secondary cauldron
  let graph = Graph.edges deps
  case Graph.topSort graph of
    Left recipeCycle ->
      Left recipeCycle
    Right (reverse -> plan) -> do
      let completeGraph = Graph.edges deps
      Right (completeGraph, (plan, fire, cauldron))

buildDepsCauldron :: Set TypeRep -> Cauldron m -> [(BeanConstructionStep, BeanConstructionStep)]
buildDepsCauldron secondary Cauldron {recipes} = do
  let makeTargetStep :: TypeRep -> BeanConstructionStep
      makeTargetStep rep =
        if rep `Set.member` secondary
          then SecondaryBean rep
          else PrimaryBean rep
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
        let bareBean = BarePrimaryBean beanRep
            boiledBean = PrimaryBean beanRep
            decos = do
              (decoIndex, decoCon) <- zip [0 :: Int ..] (Data.Foldable.toList decoCons)
              [(PrimaryBeanDeco beanRep decoIndex, decoCon)]
            beanDeps = do
              constructorEdges makeTargetStep bareBean (do constructorReps constructor)
            decoDeps = do
              (decoBean, decoCon) <- decos
              constructorEdges makeTargetStep decoBean (removeBeanFromArgs do constructorReps decoCon)
            full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [boiledBean]
            innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full)
        beanDeps ++ decoDeps ++ innerDeps

constructorEdges ::
  (TypeRep -> BeanConstructionStep) ->
  BeanConstructionStep ->
  ConstructorReps ->
  [(BeanConstructionStep, BeanConstructionStep)]
constructorEdges makeTargetStep item (ConstructorReps {argReps, regReps}) =
  -- consumers depend on their args
  ( do
      argRep <- Set.toList argReps
      let argStep = makeTargetStep argRep
      [(item, argStep)]
  )
    ++
    -- secondary beans depend on their producers
    ( do
        (regRep, _) <- Map.toList regReps
        let repStep = SecondaryBean regRep
        [(repStep, item)]
    )

followPlan ::
  (Monad m) =>
  BoiledBeans ->
  (Tree (Plan, Fire m, Cauldron m)) ->
  m (Tree BoiledBeans)
followPlan initial treecipes =
  unfoldTreeM
    ( \(initial', Node (plan, Fire {followPlanCauldron}, cauldron) rest) -> do
        newInitial' <- followPlanCauldron cauldron initial' plan
        pure (newInitial', (,) newInitial' <$> rest)
    )
    (initial, treecipes)

followPlanStep ::
  (Monad m) =>
  Cauldron m ->
  BoiledBeans ->
  BoiledBeans ->
  BeanConstructionStep ->
  m BoiledBeans
followPlanStep Cauldron {recipes} (BoiledBeans final) (BoiledBeans super) item =
  BoiledBeans <$> case item of
    BarePrimaryBean rep -> case fromJust do Map.lookup rep recipes of
      SomeBean (Bean {constructor}) -> do
        let ConstructorReps {beanRep} = constructorReps constructor
        -- We delete the beanRep before running the constructor,
        -- because if we have a self-dependency, we don't want to use the bean
        -- from a previous context (if it exists) we want the bean from final.
        -- There is a test for this.
        (super', bean) <- followConstructor constructor final (Map.delete beanRep super)
        pure do Map.insert beanRep (toDyn bean) super'
    PrimaryBeanDeco rep index -> case fromJust do Map.lookup rep recipes of
      SomeBean (Bean {decos = Decos {decoCons}}) -> do
        let decoCon = fromJust do Seq.lookup index decoCons
        let ConstructorReps {beanRep} = constructorReps decoCon
        -- Unlike before, we don't delete the beanRep before running the constructor.
        (super', bean) <- followConstructor decoCon final super
        pure do Map.insert beanRep (toDyn bean) super'
    -- \| We do nothing here, the work has been done in previous 'BarePrimaryBean' and
    -- 'PrimaryBeanDeco' steps.
    PrimaryBean _ -> pure super
    -- \| We do nothing here, secondary beans are built as a byproduct
    -- of primary beans and decorators.
    SecondaryBean _ -> pure super

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor ::
  (Monad m) =>
  Constructor m bean ->
  Map TypeRep Dynamic ->
  Map TypeRep Dynamic ->
  m (Map TypeRep Dynamic, bean)
followConstructor Constructor {constructor_ = Args {runArgs}} final super = do
  let Extractor {runExtractor} = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor final super
  results <- runArgs args
  case results of
    Regs regs bean -> do
      let inserters = cfoldMap_NP (Proxy @(Typeable `And` Monoid)) makeRegInserter regs
      pure (appEndo inserters super, bean)

newtype Extractor a where
  Extractor :: {runExtractor :: Map TypeRep Dynamic -> Map TypeRep Dynamic -> a} -> Extractor a
  deriving (Functor, Applicative) via ((->) (Map TypeRep Dynamic) `Compose` ((->) (Map TypeRep Dynamic)))

makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let runExtractor final super =
        fromJust do taste' @a super <|> taste' @a final
   in Extractor {runExtractor}

makeRegInserter :: forall a. ((Typeable `And` Monoid) a) => I a -> Endo (Map TypeRep Dynamic)
makeRegInserter (I a) =
  let appEndo dynMap = do
        let reg = fromJust do taste' @a dynMap
            dyn = toDyn (reg <> a)
        Map.insert (dynTypeRep dyn) dyn dynMap
   in Endo {appEndo}

-- | Return the resulting @bean@, if present.
taste :: forall bean. (Typeable bean) => BoiledBeans -> Maybe bean
taste BoiledBeans {beans} = taste' beans

taste' :: forall bean. (Typeable bean) => Map TypeRep Dynamic -> Maybe bean
taste' beans = do
  let rep = typeRep (Proxy @bean)
  dyn <- Map.lookup rep beans
  fromDynamic @bean dyn

-- | Sometimes the 'cook'ing process goes wrong.
data BadBeans
  = -- | The 'Cauldron' identified by 'PathToCauldron' has beans
    -- that depend on beans that can't be found either in the current 'Cauldron' or its ancestors.
    MissingDependencies PathToCauldron (Map TypeRep (Set TypeRep))
  | -- | Beans that work both as primary beans and as secondary bean
    -- registrations are disallowed.
    DoubleDutyBeans (Set TypeRep)
  | -- | Dependency cycles are disallowed by some 'Fire's.
    DependencyCycle (NonEmpty BeanConstructionStep)
  deriving stock (Show)

-- | An edge means that the source depends on the target.
--
-- The dependencies of each bean are given separatedly from its decorators.
--
-- If that level of detail is excessive, the graph can be simplified using
-- functions from the
-- [algebraic-graphs](https://hackage.haskell.org/package/algebraic-graphs-0.7/docs/Algebra-Graph-AdjacencyMap.html)
-- library.
newtype DependencyGraph = DependencyGraph {graph :: AdjacencyMap BeanConstructionStep}

toAdjacencyMap :: DependencyGraph -> AdjacencyMap BeanConstructionStep
toAdjacencyMap DependencyGraph {graph} = graph

removeSecondaryBeans :: DependencyGraph -> DependencyGraph
removeSecondaryBeans DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case SecondaryBean {} -> False; _ -> True) graph}

removeDecos :: DependencyGraph -> DependencyGraph
removeDecos DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case PrimaryBeanDeco {} -> False; _ -> True) graph}

-- Unifies 'PrimaryBean's with their respective 'BarePrimaryBean's and 'PrimaryBeanDeco's.
--
-- Also removes any self-loops.
collapsePrimaryBeans :: DependencyGraph -> DependencyGraph
collapsePrimaryBeans DependencyGraph {graph} = do
  let simplified =
        Graph.gmap
          ( \case
              BarePrimaryBean rep -> PrimaryBean rep
              PrimaryBeanDeco rep _ -> PrimaryBean rep
              other -> other
          )
          graph
      -- Is there a simpler way to removoe self-loops?
      vertices = Graph.vertexList simplified
      edges = Graph.edgeList simplified
      edgesWithoutSelfLoops =
        filter
          ( \case
              (PrimaryBean source, PrimaryBean target) -> if source == target then False else True
              _ -> True
          )
          edges
  DependencyGraph {graph = Graph.vertices vertices `Graph.overlay` Graph.edges edgesWithoutSelfLoops}

-- | See the [DOT format](https://graphviz.org/doc/info/lang.html).
exportToDot :: FilePath -> DependencyGraph -> IO ()
exportToDot filepath DependencyGraph {graph} = do
  let prettyRep =
        let p rep = Data.Text.pack do tyConName do typeRepTyCon rep
         in \case
              BarePrimaryBean rep -> p rep <> Data.Text.pack "#bare"
              PrimaryBeanDeco rep index -> p rep <> Data.Text.pack ("#deco#" ++ show index)
              PrimaryBean rep -> p rep
              SecondaryBean rep -> p rep <> Data.Text.pack "#sec"
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

-- $registrations
--
-- 'Constructor's produce a single primary bean, but sometimes they might also
-- \"register\" a number of secondary beans.
--
-- These secondary beans
-- must have 'Monoid' instances and, unlike the primary bean, can be produced by
-- more that one 'Constructor'. Their values are aggregated across all the 'Constructor's
-- that produce them. The final aggregated value can be depended upon by other 'Constructor's
-- as if it were a normal bean.
--
-- The 'Regs' type is used to represent the main bean along with the secondary
-- beans that it registers. Because constructor functions do not use the 'Regs' type,
-- a 'Packer' must be used to coax the \"tip\" of the constructor function into the
-- required shape expected by 'Constructor'.

-- | Auxiliary type which contains a primary bean along with zero or more
-- secondary \"registration\" beans. The \"registration\" beans must have
-- 'Monoid' instances.
data Regs (regs :: [Type]) bean = Regs (NP I regs) bean
  deriving (Functor)

-- | A plain @bean@ without registrations.
regs0 :: bean -> Regs '[] bean
regs0 bean = Regs Nil bean

-- | A @bean@ with one secondary bean registration.
regs1 :: reg1 -> bean -> Regs '[reg1] bean
regs1 reg1 bean = Regs (I reg1 :* Nil) bean

-- | A @bean@ with two secondary bean registrations.
regs2 :: reg1 -> reg2 -> bean -> Regs '[reg1, reg2] bean
regs2 reg1 reg2 bean = Regs (I reg1 :* I reg2 :* Nil) bean

-- | A @bean@ with three secondary bean registrations.
regs3 :: reg1 -> reg2 -> reg3 -> bean -> Regs '[reg1, reg2, reg3] bean
regs3 reg1 reg2 reg3 bean = Regs (I reg1 :* I reg2 :* I reg3 :* Nil) bean

-- | Applies a transformation to the tip of a curried function, coaxing
-- it into the shape expected by a 'Constructor'.
--
-- * For pure constructors without registrations, try 'value'.
--
-- * For effectful constructors without registrations, try 'effect'.
--
-- More complex cases might require 'valueWith', 'effectWith', or working with
-- the 'Packer' constructor itself.
newtype Packer m regs bean r = Packer (r -> m (Regs regs bean))

runPacker :: Packer m regs bean r -> r -> m (Regs regs bean)
runPacker (Packer f) = f

instance Contravariant (Packer m regs bean) where
  contramap f (Packer p) = Packer (p . f)

-- | For pure constructors that return the @bean@ directly, and do not register
-- secondary beans.
value :: (Applicative m) => Packer m '[] bean bean
value = Packer \bean -> pure do regs0 bean

-- | For effectul constructors that return an @m bean@ initialization action,
-- and do not register secondary beans.
effect :: (Applicative m) => Packer m '[] bean (m bean)
effect = Packer \action -> do fmap regs0 action

valueWith ::
  (Applicative m, All (Typeable `And` Monoid) regs) =>
  -- | Massage the pure value at the tip of the constructor into a 'Regs'.
  (r -> Regs regs bean) ->
  Packer m regs bean r
valueWith f = Packer do pure . f

effectWith ::
  (Applicative m, All (Typeable `And` Monoid) regs) =>
  -- | Massage the value returned by the action at the tip of the constructor into a 'Regs'.
  (r -> Regs regs bean) ->
  Packer m regs bean (m r)
effectWith f = Packer do fmap f

-- | Take a curried function that constructs a bean, uncurry it recursively and
-- then apply a 'Packer' to its tip, resulting in a 'Constructor'.
pack ::
  forall (args :: [Type]) r curried regs bean m.
  ( MulticurryableF args r curried (IsFunction curried),
    All Typeable args,
    All (Typeable `And` Monoid) regs
  ) =>
  -- | Fit the outputs of the constructor into the auxiliary 'Regs' type.
  --
  -- See 'regs1' and similar functions.
  Packer m regs bean r ->
  -- | Action returning a function ending in @r@, some datatype containing
  -- @regs@ and @bean@ values.
  curried ->
  Constructor m bean
pack packer curried = Constructor do runPacker packer <$> do argsN curried

-- | Slightly simpler version of 'pack' for @0@-argument functions.
pack0 ::
  (All (Typeable `And` Monoid) regs) =>
  Packer m regs bean r ->
  r ->
  Constructor m bean
pack0 packer r = Constructor do Args @'[] \Nil -> runPacker packer r

-- | Slightly simpler version of 'pack' for @1@-argument functions.
pack1 ::
  forall arg1 r m regs bean.
  (Typeable arg1, All (Typeable `And` Monoid) regs) =>
  Packer m regs bean r ->
  -- | @1@-argument constructor
  (arg1 -> r) ->
  Constructor m bean
pack1 packer f = Constructor do Args @'[arg1] \(I arg1 :* Nil) -> runPacker packer (f arg1)

-- | Slightly simpler version of 'pack' for @2@-argument functions.
pack2 ::
  forall arg1 arg2 r m regs bean.
  (Typeable arg1, Typeable arg2, All (Typeable `And` Monoid) regs) =>
  Packer m regs bean r ->
  -- | @2@-argument constructor
  (arg1 -> arg2 -> r) ->
  Constructor m bean
pack2 packer f = Constructor do Args @[arg1, arg2] \(I arg1 :* I arg2 :* Nil) -> runPacker packer (f arg1 arg2)

-- | Slightly simpler version of 'pack' for @3@-argument functions.
pack3 ::
  forall arg1 arg2 arg3 r m regs bean.
  (Typeable arg1, Typeable arg2, Typeable arg3, All (Typeable `And` Monoid) regs) =>
  Packer m regs bean r ->
  -- | @3@-argument constructor
  (arg1 -> arg2 -> arg3 -> r) ->
  Constructor m bean
pack3 packer f = Constructor do Args @[arg1, arg2, arg3] \(I arg1 :* I arg2 :* I arg3 :* Nil) -> runPacker packer (f arg1 arg2 arg3)

nonEmptyToTree :: NonEmpty a -> Tree a
nonEmptyToTree = \case
  a Data.List.NonEmpty.:| [] -> Node a []
  a Data.List.NonEmpty.:| (b : rest) -> Node a [nonEmptyToTree (b Data.List.NonEmpty.:| rest)]

unsafeTreeToNonEmpty :: Tree a -> NonEmpty a
unsafeTreeToNonEmpty = \case
  Node a [] -> a Data.List.NonEmpty.:| []
  Node a [b] -> Data.List.NonEmpty.cons a (unsafeTreeToNonEmpty b)
  _ -> error "tree not list-shaped"

-- | This is a copy of the @Managed@ type from the
-- [managed](https://hackage.haskell.org/package/managed) package, with a dodgy
-- 'Control.Monad.Fix.MonadFix' instance tacked on.
newtype Managed a = Managed (forall b. (a -> IO b) -> IO b)

-- | Build a 'Managed' value from a @withFoo@-style resource-handling function
-- that accepts a continuation, like 'System.IO.withFile'.
--
-- Passing functions that do weird things like running their continuation
-- /twice/ will tear apart the fabric of reality. Why would you want to do that?
-- Pass only @withFoo@-style functions.
managed :: (forall r. (a -> IO r) -> IO r) -> Managed a
managed = Managed

-- | This instance is a little dodgy (continuation-like monads don't have proper
-- 'MonadFix' instances) but it is nevertheless useful because it lets us use
-- 'Managed' with 'allowSelfDeps'. Follow the recommendations for the 'managed'
-- function.
--
-- [\"if you embrace the unsafety, it could be a fun way to tie knots.\"](https://stackoverflow.com/questions/25827227/why-cant-there-be-an-instance-of-monadfix-for-the-continuation-monad#comment113010373_63906214)
instance MonadFix Managed where
  -- https://stackoverflow.com/a/63906214
  -- See also the implementation for fixIO https://hackage.haskell.org/package/base-4.19.0.0/docs/src/System.IO.html#fixIO
  mfix f = Managed \k -> do
    m <- newEmptyMVar
    x <-
      unsafeDupableInterleaveIO
        ( readMVar m `catch` \BlockedIndefinitelyOnMVar ->
            throwIO FixIOException
        )
    unManage (f x) \x' -> do
      putMVar m x'
      k x'
    where
      unManage (Managed a) = a

-- | Make use of the managed resource by supplying a callback.
with :: Managed a -> (a -> IO b) -> IO b
with (Managed r) = r

instance Functor Managed where
  fmap f (Managed m) = Managed (\k -> m (\x -> k (f x)))
  {-# INLINE fmap #-}

instance Applicative Managed where
  pure x = Managed (\k -> k x)
  {-# INLINE pure #-}
  Managed f <*> Managed g = Managed (\bfr -> f (\ab -> g (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}

instance Monad Managed where
  return = pure
  {-# INLINE return #-}
  m >>= k = Managed (\c -> with m (\a -> with (k a) c))
  {-# INLINE (>>=) #-}

instance MonadIO Managed where
  liftIO m = Managed \return_ -> do
    a <- m
    return_ a
  {-# INLINE liftIO #-}
