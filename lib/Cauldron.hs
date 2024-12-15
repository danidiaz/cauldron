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

-- | This is a library for performing dependency injection. It's an alternative
-- to manually wiring your functions and passing all required parameters
-- explicitly. Instead of that, you throw your functions into a 'Cauldron', which wires
-- them for you, guiding itself by the types.
--
-- Wiring errors are detected at runtime, not at compile time.
--
-- This library should be used at the ["composition root"](https://stackoverflow.com/questions/6277771/what-is-a-composition-root-in-the-context-of-dependency-injection) of the application,
-- and only there: the components we are wiring together need not be aware that the library exists.
--
-- >>> :{
-- data A = A deriving Show
-- data B = B deriving Show
-- data C = C deriving Show
-- makeA :: A
-- makeA = A
-- makeB :: A -> B
-- makeB = \_ -> B
-- makeC :: A -> B -> IO C
-- makeC = \_ _ -> pure C
-- :}
--
-- >>> :{
-- do
--   let cauldron :: Cauldron IO
--       cauldron =
--         emptyCauldron
--         & insert @A do recipe do pack value makeA
--         & insert @B do recipe do pack value makeB
--         & insert @C do recipe do pack effect makeC
--       Right (_ :: DependencyGraph, action) = cook forbidDepCycles cauldron
--   beans <- action
--   pure do taste @C beans
-- :}
-- Just C
module Cauldron
  ( -- * Filling the cauldron
    Cauldron,
    insert,
    adjust,
    delete,
    hoistCauldron,

    -- * Recipes
    Recipe (..),
    Recipe_ (..),
    ToRecipe (..),
    hoistRecipe,
    SomeRecipe,
    someRecipe,
    fromSomeRecipeList,

    -- * Cooking the beans
    cook,
    cookNonEmpty,
    cookTree,

    -- ** How loopy can we get?
    Fire,
    forbidDepCycles,
    allowSelfDeps,

    -- ** Tasting the results
    RecipeError (..),
    PathToCauldron,

    -- ** Drawing deps
    DependencyGraph,
    exportToDot,
    defaultStepToText,
    BeanConstructionStep (..),
    removeSecondaryBeans,
    removeDecos,
    collapsePrimaryBeans,
    toAdjacencyMap,

    -- * Re-exported
    Constructor,
    constructor,
    effectfulConstructor,
    constructorWithRegs,
    effectfulConstructorWithRegs,
    hoistConstructor,
    Args,
    arg,
    fillArgs,
    reg,
    Regs,
    Beans,
    taste,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Cauldron.Beans qualified
import Cauldron.Constructor
import Control.Applicative
import Control.Monad.Fix
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.ByteString qualified as Data.List
import Data.Dynamic
import Data.Either (fromRight)
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Functor.Compose
import Data.Functor.Contravariant
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
import Data.Semigroup qualified
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
import GHC.IsList
import Multicurryable
import Type.Reflection qualified

-- | A map of 'Bean' recipes. Parameterized by the monad @m@ in which the 'Bean'
-- 'Constructor's might have effects.
newtype Cauldron m where
  Cauldron :: {recipes :: Map TypeRep (SomeRecipe m)} -> Cauldron m

-- | Union of two 'Cauldron's, right-biased: prefers values from the /right/ cauldron when
-- both contain the same bean. (Note that 'Data.Map.Map' is left-biased.)
instance Semigroup (Cauldron m) where
  Cauldron {recipes = r1} <> Cauldron {recipes = r2} = Cauldron do Map.unionWith (flip const) r1 r2

instance Monoid (Cauldron m) where
  mempty = Cauldron do Map.empty

instance IsList (Cauldron m) where
  type Item (Cauldron m) = SomeRecipe m
  toList (Cauldron {recipes}) = Map.elems recipes
  fromList = fromSomeRecipeList

-- | Change the monad used by the beans in the 'Cauldron'.
hoistCauldron :: (forall x. m x -> n x) -> Cauldron m -> Cauldron n
hoistCauldron f (Cauldron {recipes}) = Cauldron {recipes = hoistSomeRecipe f <$> recipes}

data SomeRecipe m where
  SomeRecipe :: (Typeable bean) => Recipe m bean -> SomeRecipe m

someRecipe :: forall bean recipe m. (Typeable bean, ToRecipe recipe) => recipe m bean -> SomeRecipe m
someRecipe recipe = SomeRecipe (toRecipe recipe)

fromSomeRecipeList :: [SomeRecipe m] -> Cauldron m
fromSomeRecipeList =
  foldl
    do \c (SomeRecipe r) -> insert r c
    do mempty

hoistSomeRecipe :: (forall x. m x -> n x) -> SomeRecipe m -> SomeRecipe n
hoistSomeRecipe f (SomeRecipe bean) = SomeRecipe do hoistRecipe f bean

-- | A bean recipe, to be inserted into a 'Cauldron'.
data Recipe m bean where
  Recipe ::
    { -- | How to build the bean itself.
      bean :: Constructor m bean,
      -- | How to build the decorators that wrap the bean. There might be no decorators.
      decos :: [Constructor m bean]
    } ->
    Recipe m bean

newtype Recipe_ m bean where
  Recipe_ ::
    { -- | How to build the bean itself.
      bean :: Constructor m bean
    } ->
    Recipe_ m bean

class ToRecipe c where
  toRecipe :: c m bean -> Recipe m bean

instance ToRecipe Recipe where
  toRecipe = id

instance ToRecipe Recipe_ where
  toRecipe (Recipe_ {bean}) = Recipe {bean, decos = []}

instance ToRecipe Constructor where
  toRecipe c = Recipe {bean = c, decos = []}

-- | Change the monad used by the bean\'s 'Constructor' and its 'Decos'.
hoistRecipe :: (forall x. m x -> n x) -> Recipe m bean -> Recipe n bean
hoistRecipe f (Recipe {bean, decos}) =
  Recipe
    { bean = hoistConstructor f bean,
      decos = hoistConstructor f <$> decos
    }

-- | A 'Recipe' without decorators, having only the main bean.
recipe :: Constructor m a -> Recipe m a
recipe bean = Recipe {bean, decos = mempty}

-- $decos
--
-- Decorators are 'Constructor's which, instead constructing the original
-- version of a bean, they modify it in some way (but without changing its
-- type). Because they modify the bean, typically decorators will take the bean
-- as an argument.
--
-- Decorators can have other dependencies beyond the modified bean.
--
-- When the bean is a record-of-functions, decorators can be used to
-- add behaviors like caching, logging... to the functions.
--
--
-- >>> :{
-- newtype Foo = Foo { sayFoo :: IO () }
-- makeFoo :: Foo
-- makeFoo = Foo { sayFoo = putStrLn "foo" }
-- makeFooDeco1 :: Foo -> Foo
-- makeFooDeco1 Foo { sayFoo } = Foo { sayFoo = putStrLn "deco1 enter" >> sayFoo >> putStrLn "deco1 exit" }
-- makeFooDeco2 :: Foo -> IO Foo
-- makeFooDeco2 Foo { sayFoo } = putStrLn "deco2 init" >> pure Foo { sayFoo = putStrLn "deco2 enter" >> sayFoo >> putStrLn "deco2 exit" }
-- :}
--
-- >>> :{
-- do
--   let cauldron :: Cauldron IO
--       cauldron =
--         emptyCauldron
--         & insert @Foo
--           Recipe {
--             bean = pack value makeFoo,
--             decos = [
--                  pack value makeFooDeco1,
--                  pack effect makeFooDeco2
--               ]
--           }
--       Right (_ :: DependencyGraph, action) = cook forbidDepCycles cauldron
--   beans <- action
--   let Just Foo {sayFoo} = taste beans
--   sayFoo
-- :}
-- deco2 init
-- deco2 enter
-- deco1 enter
-- foo
-- deco1 exit
-- deco2 exit

-- $constructors
--
-- The bean-producing or bean-decorating functions that we want to wire need to be
-- coaxed into a 'Constructor' value before creating a 'Recipe' recipe and adding it to the 'Cauldron'.
--
-- If your aren't dealing with secondary beans, don't sweat it: use @pack value@ for pure
-- constructors functions and @pack effect@ for effectful ones. That should be enough.

-- | A way of building some @bean@ value, potentially requiring some
-- dependencies, potentially returning some secondary beans
-- along the primary @bean@ result, and also potentially requiring some
-- initialization effect in a monad @m@.
--
-- Note that only the type of the primary @bean@ is reflected in the
-- 'Constructor' type. Those of the dependencies and secondary beans are not.
--
-- A typical initialization monad will be 'IO', used for example to create
-- mutable references that the bean will use internally. Sometimes the a
-- constructor will allocate resources with bracket-like operations, and in that
-- case a monad like 'Managed' might be needed instead.
data ConstructorReps where
  ConstructorReps ::
    { beanRep :: TypeRep,
      argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

-- | Put a recipe for a 'Recipe' into the 'Cauldron'.
--
-- Only one recipe is allowed for each different @bean@ type, so 'insert' for a
-- @bean@ will overwrite previous recipes for that type.
insert ::
  forall (bean :: Type) m recipe.
  (Typeable bean, ToRecipe recipe) =>
  recipe m bean ->
  Cauldron m ->
  Cauldron m
insert aRecipe Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
  Cauldron {recipes = Map.insert rep (SomeRecipe (toRecipe aRecipe)) recipes}

-- | Tweak an already existing 'Recipe' recipe.
adjust ::
  forall bean m.
  (Typeable bean) =>
  (Recipe m bean -> Recipe m bean) ->
  Cauldron m ->
  Cauldron m
adjust f (Cauldron {recipes}) = do
  let rep = typeRep (Proxy @bean)
  Cauldron
    { recipes =
        Map.adjust
          do
            \(SomeRecipe (r :: Recipe m a)) ->
              case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                Nothing -> error "should never happen"
                Just Refl -> SomeRecipe (f r)
          rep
          recipes
    }

delete ::
  forall m.
  TypeRep ->
  Cauldron m ->
  Cauldron m
delete tr Cauldron {recipes} =
  Cauldron {recipes = Map.delete tr recipes}

-- | Strategy for dealing with dependency cycles.
--
-- (Terrible uninformative name caused by a metaphor stretched too far.)
data Fire m = Fire
  { shouldOmitDependency :: (BeanConstructionStep, BeanConstructionStep) -> Bool,
    followPlanCauldron ::
      Cauldron m ->
      Beans ->
      Plan ->
      m Beans
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

-- | Forbid any kind of cyclic dependencies between beans. This is probably what you want.
forbidDepCycles :: (Monad m) => Fire m
forbidDepCycles =
  Fire
    { shouldOmitDependency = \_ -> False,
      followPlanCauldron = \cauldron initial plan ->
        Data.Foldable.foldlM
          do followPlanStep cauldron mempty
          initial
          plan
    }

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330

-- | This function DOESN'T return the bean rep itself in the argreps.
constructorReps :: forall bean m. (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps c =
  ConstructorReps
    { beanRep = typeRep (Proxy @bean),
      argReps = getArgReps c,
      regReps =
        c
          & getRegReps
          & Set.map (\mtr@(SomeMonoidTypeRep tr) -> Data.Semigroup.Arg (Type.Reflection.SomeTypeRep tr) (toDyn (someMonoidTypeRepMempty mtr)))
          & Map.fromArgSet
    }
  where
    someMonoidTypeRepMempty :: SomeMonoidTypeRep -> Dynamic
    someMonoidTypeRepMempty (SomeMonoidTypeRep @t _) = toDyn (mempty @t)

type Plan = [BeanConstructionStep]

-- | A step in the construction of a bean value.
data BeanConstructionStep
  = -- | Undecorated bean.
    BarePrimaryBean TypeRep
  | -- | Apply the decorator with the given index. Comes after the 'BarePrimaryBean' and all 'PrimaryBeanDeco's with a lower index value.
    PrimaryBeanDeco TypeRep Int
  | -- | Final, fully decorated version of a bean. If there are no decorators, comes directly after 'BarePrimaryBean'.
    PrimaryBean TypeRep
  | -- | Beans that are secondary registrations of a 'Constructor' and which are aggregated monoidally.
    SecondaryBean TypeRep
  deriving stock (Show, Eq, Ord)

-- | Build the beans using the recipes stored in the 'Cauldron'.
cook ::
  forall m.
  (Monad m) =>
  Fire m ->
  Cauldron m ->
  Either RecipeError (DependencyGraph, m Beans)
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
  Either RecipeError (NonEmpty DependencyGraph, m (NonEmpty Beans))
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
  Either RecipeError (Tree DependencyGraph, m (Tree Beans))
cookTree (treecipes) = do
  accumMap <- first DoubleDutyBeans do checkNoDoubleDutyBeans (snd <$> treecipes)
  () <- first (uncurry MissingDependencies) do checkMissingDeps (Map.keysSet accumMap) (snd <$> treecipes)
  treeplan <- first DependencyCycle do buildPlans (Map.keysSet accumMap) treecipes
  Right
    ( treeplan <&> \(graph, _) -> DependencyGraph {graph},
      followPlan (fromDynList (Data.Foldable.toList accumMap)) (snd <$> treeplan)
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
    do \rep aRecipe -> (recipeRegs aRecipe, Set.singleton rep)
    recipes

-- | Returns the accumulators, not the main bean
recipeRegs :: SomeRecipe m -> Map TypeRep Dynamic
recipeRegs (SomeRecipe (Recipe {bean, decos})) = do
  let extractRegReps = (.regReps) . constructorReps
  extractRegReps bean
    <> foldMap extractRegReps decos

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
  let missingMap = (`Map.mapMaybe` recipes) \someBean -> do
        let missing = Set.filter (`Set.notMember` available) do demanded someBean
        if Set.null missing
          then Nothing
          else Just missing
  if not (Map.null missingMap)
    then Left missingMap
    else Right ()
  where
    demanded :: SomeRecipe m -> Set TypeRep
    demanded (SomeRecipe Recipe {bean, decos}) =
      ( Set.fromList do
          let ConstructorReps {argReps = beanArgReps} = constructorReps bean
          Set.toList beanArgReps ++ do
            decoCon <- decos
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
     ( SomeRecipe
         ( Recipe
             { bean = bean :: Constructor m bean,
               decos = decoCons
             }
           )
       ) -> do
        let bareBean = BarePrimaryBean beanRep
            boiledBean = PrimaryBean beanRep
            decos = do
              (decoIndex, decoCon) <- zip [0 :: Int ..] (Data.Foldable.toList decoCons)
              [(PrimaryBeanDeco beanRep decoIndex, decoCon)]
            beanDeps = do
              constructorEdges makeTargetStep bareBean (do constructorReps bean)
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
  Beans ->
  (Tree (Plan, Fire m, Cauldron m)) ->
  m (Tree Beans)
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
  Beans ->
  Beans ->
  BeanConstructionStep ->
  m Beans
followPlanStep Cauldron {recipes} final super item =
  case item of
    BarePrimaryBean rep -> case fromJust do Map.lookup rep recipes of
      SomeRecipe (Recipe {bean = beanConstructor}) -> do
        let ConstructorReps {beanRep} = constructorReps beanConstructor
        -- We delete the beanRep before running the bean,
        -- because if we have a self-dependency, we don't want to use the bean
        -- from a previous context (if it exists) we want the bean from final.
        -- There is a test for this.
        (super', bean) <- followConstructor beanConstructor final (Cauldron.Beans.delete beanRep super)
        pure do Cauldron.Beans.insert bean super'
    PrimaryBeanDeco rep index -> case fromJust do Map.lookup rep recipes of
      SomeRecipe (Recipe {decos}) -> do
        let decoCon = decos Data.List.!! index
        -- Unlike before, we don't delete the beanRep before running the constructor.
        (super', bean) <- followConstructor decoCon final super
        pure do Cauldron.Beans.insert bean super'
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
  Beans ->
  Beans ->
  m (Beans, bean)
followConstructor c final super = do
  let Right action = runConstructor [super, final] c
  (regs, bean) <- action
  pure (Cauldron.Beans.unionBeansMonoidally (getRegReps c) super regs, bean)

-- | Sometimes the 'cook'ing process goes wrong.
data RecipeError
  = -- | The 'Cauldron' identified by 'PathToCauldron' has beans
    -- that depend on beans that can't be found either in the current 'Cauldron' or its ancestors.
    MissingDependencies PathToCauldron (Map TypeRep (Set TypeRep))
  | -- | Beans that work both as primary beans and as secondary beans
    -- are disallowed.
    DoubleDutyBeans (Set TypeRep)
  | -- | Dependency cycles are disallowed by some 'Fire's.
    DependencyCycle (NonEmpty BeanConstructionStep)
  deriving stock (Show)

-- | An edge means that the source depends on the target.
--
-- The dependencies of each bean are given separatedly from its decorators.
newtype DependencyGraph = DependencyGraph {graph :: AdjacencyMap BeanConstructionStep}

-- | Conversion to a graph type
-- from the
-- [algebraic-graphs](https://hackage.haskell.org/package/algebraic-graphs-0.7/docs/Algebra-Graph-AdjacencyMap.html)
-- library for further processing.
toAdjacencyMap :: DependencyGraph -> AdjacencyMap BeanConstructionStep
toAdjacencyMap DependencyGraph {graph} = graph

removeSecondaryBeans :: DependencyGraph -> DependencyGraph
removeSecondaryBeans DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case SecondaryBean {} -> False; _ -> True) graph}

removeDecos :: DependencyGraph -> DependencyGraph
removeDecos DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case PrimaryBeanDeco {} -> False; _ -> True) graph}

-- | Unifies 'PrimaryBean's with their respective 'BarePrimaryBean's and 'PrimaryBeanDeco's.
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
exportToDot :: (BeanConstructionStep -> Data.Text.Text) -> FilePath -> DependencyGraph -> IO ()
exportToDot prettyRep filepath DependencyGraph {graph} = do
  let dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          graph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)

defaultStepToText :: BeanConstructionStep -> Data.Text.Text
defaultStepToText =
  let p rep = Data.Text.pack do show rep
   in \case
        BarePrimaryBean rep -> p rep <> Data.Text.pack "#bare"
        PrimaryBeanDeco rep index -> p rep <> Data.Text.pack ("#deco#" ++ show index)
        PrimaryBean rep -> p rep
        SecondaryBean rep -> p rep <> Data.Text.pack "#sec"

nonEmptyToTree :: NonEmpty a -> Tree a
nonEmptyToTree = \case
  a Data.List.NonEmpty.:| [] -> Node a []
  a Data.List.NonEmpty.:| (b : rest) -> Node a [nonEmptyToTree (b Data.List.NonEmpty.:| rest)]

unsafeTreeToNonEmpty :: Tree a -> NonEmpty a
unsafeTreeToNonEmpty = \case
  Node a [] -> a Data.List.NonEmpty.:| []
  Node a [b] -> Data.List.NonEmpty.cons a (unsafeTreeToNonEmpty b)
  _ -> error "tree not list-shaped"

-- $setup
-- >>> :set -XBlockArguments
-- >>> :set -Wno-incomplete-uni-patterns
-- >>> import Data.Functor.Identity
-- >>> import Data.Function ((&))
-- >>> import Data.Monoid
