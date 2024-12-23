{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
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
--       cauldron = [
--           recipe @A $ val do wire makeA,
--           recipe @B $ val do wire makeB,
--           recipe @C $ eff do wire makeC
--         ]
--       Right (_ :: DependencyGraph, action) = cook forbidDepCycles cauldron
--   beans <- action
--   pure do taste @C beans
-- :}
-- Just C
module Cauldron
  ( -- * Filling the cauldron
    Cauldron,
    empty,
    insert,
    adjust,
    delete,
    keysSet,
    restrictKeys,
    hoistCauldron,
    fromRecipeList,
    toRecipeMap,

    -- * Recipes
    Recipe (..),
    ToRecipe,
    hoistRecipe,
    fromDecoList,
    (Data.Sequence.|>),
    (Data.Sequence.<|),
    SomeRecipe,
    recipe,
    withRecipe,
    getRecipeCallStacks,

    -- * Constructor
    Constructor,
    val,
    val',
    val_,
    eff,
    eff',
    eff_,
    hoistConstructor,
    getConstructorArgs,
    getConstructorCallStack,

    -- * Cooking the beans
    cook,
    cookNonEmpty,
    cookTree,

    -- ** How loopy can we get?
    Fire,
    forbidDepCycles,
    allowDepCycles,
    allowSelfDeps,

    -- ** Tasting the results
    RecipeError (..),
    MissingDependencies (..),
    DoubleDutyBeans (..),
    DependencyCycle (..),
    prettyRecipeError,
    prettyRecipeErrorLines,

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

    -- Args,
    wire,
    Beans,
    taste,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Cauldron.Args
import Cauldron.Beans qualified
import Control.Monad.Fix
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Kind
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Semigroup qualified
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Tree
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exception (CallStack, prettyCallStackLines)
import GHC.IsList
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Type.Reflection qualified

-- | A map of 'Bean' recipes. Parameterized by the monad @m@ in which the 'Bean'
-- 'Constructor's might have effects.
newtype Cauldron m where
  Cauldron :: {recipes :: Map TypeRep (SomeRecipe m)} -> Cauldron m

empty :: Cauldron m
empty = Cauldron {recipes = Map.empty}

-- | Union of two 'Cauldron's, right-biased: prefers values from the /right/ cauldron when
-- both contain the same bean. (Note that 'Data.Map.Map' is left-biased.)
instance Semigroup (Cauldron m) where
  Cauldron {recipes = r1} <> Cauldron {recipes = r2} = Cauldron do Map.unionWith (flip const) r1 r2

instance Monoid (Cauldron m) where
  mempty = Cauldron do Map.empty

instance IsList (Cauldron m) where
  type Item (Cauldron m) = SomeRecipe m
  toList (Cauldron {recipes}) = Map.elems recipes
  fromList = fromRecipeList

-- | Change the monad used by the beans in the 'Cauldron'.
hoistCauldron :: (forall x. m x -> n x) -> Cauldron m -> Cauldron n
hoistCauldron f (Cauldron {recipes}) = Cauldron {recipes = hoistSomeRecipe f <$> recipes}

data SomeRecipe m where
  SomeRecipe :: (Typeable bean) => NonEmpty CallStack -> Recipe m bean -> SomeRecipe m

recipe :: forall bean recipe m. (Typeable bean, ToRecipe recipe, HasCallStack) => recipe m bean -> SomeRecipe m
recipe theRecipe = withFrozenCallStack do
  let initialStacks = Data.List.NonEmpty.singleton callStack
  SomeRecipe initialStacks (toRecipe theRecipe)

withRecipe :: forall m r. (forall bean. (Typeable bean) => NonEmpty CallStack -> Recipe m bean -> r) -> SomeRecipe m -> r
withRecipe f (SomeRecipe stacks theRecipe) = f stacks theRecipe

fromRecipeList :: [SomeRecipe m] -> Cauldron m
fromRecipeList =
  foldl
    do \c sr -> withRecipe inserter sr c
    do empty
  where
    -- Here we take care to preserve the initial call stacks
    inserter :: forall bean m. (Typeable bean) => NonEmpty CallStack -> Recipe m bean -> Cauldron m -> Cauldron m
    inserter stacks r Cauldron {recipes} =
      Cauldron
        { recipes = Map.insert (typeRep (Proxy @bean)) (SomeRecipe stacks r) recipes
        }

toRecipeMap :: Cauldron m -> Map TypeRep (SomeRecipe m)
toRecipeMap Cauldron {recipes} = recipes

hoistSomeRecipe :: (forall x. m x -> n x) -> SomeRecipe m -> SomeRecipe n
hoistSomeRecipe f (SomeRecipe stack bean) = SomeRecipe stack do hoistRecipe f bean

-- | A bean recipe, to be inserted into a 'Cauldron'.
data Recipe m bean where
  Recipe ::
    { -- | How to build the bean itself.
      bean :: Constructor m bean,
      -- | How to build the decorators that wrap the bean. There might be no decorators.
      decos :: Seq (Constructor m bean)
    } ->
    Recipe m bean

fromDecoList :: [Constructor m bean] -> Seq (Constructor m bean)
fromDecoList = Data.Sequence.fromList

class ToRecipe c where
  toRecipe :: c m bean -> Recipe m bean

instance ToRecipe Recipe where
  toRecipe = id

instance ToRecipe Constructor where
  toRecipe c = Recipe {bean = c, decos = Data.Sequence.empty}

-- | Change the monad used by the bean\'s 'Constructor' and its 'Decos'.
hoistRecipe :: (forall x. m x -> n x) -> Recipe m bean -> Recipe n bean
hoistRecipe f (Recipe {bean, decos}) =
  Recipe
    { bean = hoistConstructor f bean,
      decos = hoistConstructor f <$> decos
    }

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
--       cauldron = [
--           recipe @Foo $ Recipe {
--             bean = val do wire makeFoo,
--             decos = [
--                  val do wire makeFooDeco1,
--                  eff do wire makeFooDeco2
--               ]
--           }
--         ]
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
  (Typeable bean, ToRecipe recipe, HasCallStack) =>
  recipe m bean ->
  Cauldron m ->
  Cauldron m
insert aRecipe Cauldron {recipes} = withFrozenCallStack do
  let rep = typeRep (Proxy @bean)
  let initialStacks = Data.List.NonEmpty.singleton callStack
  Cauldron {recipes = Map.insert rep (SomeRecipe initialStacks (toRecipe aRecipe)) recipes}

-- | Tweak an already existing 'Recipe' recipe.
adjust ::
  forall bean m.
  (Typeable bean, HasCallStack) =>
  (Recipe m bean -> Recipe m bean) ->
  Cauldron m ->
  Cauldron m
adjust f (Cauldron {recipes}) = withFrozenCallStack do
  let rep = typeRep (Proxy @bean)
  Cauldron
    { recipes =
        Map.adjust
          do
            \(SomeRecipe stacks (r :: Recipe m a)) ->
              case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                Nothing -> error "should never happen"
                Just Refl -> SomeRecipe (Data.List.NonEmpty.appendList stacks [callStack]) (f r)
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
      Set TypeRep ->
      Beans ->
      Plan ->
      m Beans
  }

removeBeanFromArgs :: ConstructorReps -> ConstructorReps
removeBeanFromArgs ConstructorReps {argReps, regReps, beanRep} =
  ConstructorReps {argReps = Set.delete beanRep argReps, regReps, beanRep}

-- | Forbid any kind of cyclic dependencies between beans. This is probably what you want.
forbidDepCycles :: (Monad m) => Fire m
forbidDepCycles =
  Fire
    { shouldOmitDependency = \_ -> False,
      followPlanCauldron = \cauldron _secondaryBeanReps initial plan ->
        Data.Foldable.foldlM
          do followPlanStep (\_ -> id) (\_ -> id) cauldron mempty
          initial
          plan
    }

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
      followPlanCauldron = \cauldron _secondaryBeanReps initial plan ->
        mfix do
          \final ->
            Data.Foldable.foldlM
              do followPlanStep Cauldron.Beans.delete (\_ -> id) cauldron final
              initial
              plan
    }

allowDepCycles :: (MonadFix m) => Fire m
allowDepCycles =
  Fire
    { shouldOmitDependency = \case
        (BarePrimaryBean _, PrimaryBean _) -> True
        (PrimaryBeanDeco _ _, PrimaryBean _) -> True
        _ -> False,
      followPlanCauldron = \cauldron secondaryBeanReps initial plan -> do
        let makeBareView _ = (`Cauldron.Beans.restrictKeys` secondaryBeanReps)
        let makeDecoView tr = (`Cauldron.Beans.restrictKeys` (Set.insert tr secondaryBeanReps))
        mfix do
          \final ->
            Data.Foldable.foldlM
              do followPlanStep makeBareView makeDecoView cauldron final
              initial
              plan
    }

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330

-- | This function DOESN'T return the bean rep itself in the argreps.
constructorReps :: forall bean m. (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps (getConstructorArgs -> c) =
  ConstructorReps
    { beanRep = typeRep (Proxy @bean),
      argReps = getArgsReps c,
      regReps =
        c
          & getRegsReps
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
  accumMap <- first DoubleDutyBeansError do checkNoDoubleDutyBeans (snd <$> treecipes)
  () <- first MissingDependenciesError do checkMissingDeps (Map.keysSet accumMap) (snd <$> treecipes)
  treeplan <- first DependencyCycleError do buildPlans (Map.keysSet accumMap) treecipes
  Right
    ( treeplan <&> \(graph, _) -> DependencyGraph {graph},
      followPlan (fromDynList (Data.Foldable.toList accumMap)) (snd <$> treeplan)
    )

newtype DoubleDutyBeans = DoubleDutyBeans (Map TypeRep (CallStack, CallStack))
  deriving stock (Show)

checkNoDoubleDutyBeans ::
  Tree (Cauldron m) ->
  Either DoubleDutyBeans (Map TypeRep Dynamic)
checkNoDoubleDutyBeans treecipes = do
  let (accumMap, beanSet) = cauldronTreeRegs treecipes
  let common = Map.intersectionWith (,) (fst <$> accumMap) beanSet
  if not (Map.null common)
    then Left $ DoubleDutyBeans common
    else Right $ snd <$> accumMap

cauldronTreeRegs :: Tree (Cauldron m) -> (Map TypeRep (CallStack, Dynamic), Map TypeRep CallStack)
cauldronTreeRegs = foldMap cauldronRegs

cauldronRegs :: Cauldron m -> (Map TypeRep (CallStack, Dynamic), Map TypeRep CallStack)
cauldronRegs Cauldron {recipes} =
  Map.foldMapWithKey
    do \rep aRecipe -> (recipeRegs aRecipe, Map.singleton rep (Data.List.NonEmpty.head $ getRecipeCallStacks aRecipe))
    recipes

-- | Returns the accumulators, not the main bean
recipeRegs :: SomeRecipe m -> Map TypeRep (CallStack, Dynamic)
recipeRegs (SomeRecipe _ (Recipe {bean, decos})) = do
  let extractRegReps c = (getConstructorCallStack c,) <$> (\ConstructorReps {regReps} -> regReps) (constructorReps c)
  extractRegReps bean
    <> foldMap extractRegReps decos

data MissingDependencies = MissingDependencies CallStack TypeRep (Set TypeRep)
  deriving stock (Show)

checkMissingDeps ::
  -- | accums
  Set TypeRep ->
  Tree (Cauldron m) ->
  Either MissingDependencies ()
checkMissingDeps accums treecipes = do
  let decoratedTreecipes = decorate (Map.empty, treecipes)
      missing =
        decoratedTreecipes <&> \(available, requested) ->
          do checkMissingDepsCauldron accums (Map.keysSet available) requested
  sequence_ missing
  where
    decorate ::
      (Map TypeRep (SomeRecipe m), Tree (Cauldron m)) ->
      Tree (Map TypeRep (SomeRecipe m), Cauldron m)
    decorate = unfoldTree
      do
        \(acc, Node (current@Cauldron {recipes}) rest) ->
          let -- current level has priority
              newAcc = recipes `Map.union` acc
              newSeeds = do
                z <- rest
                [(newAcc, z)]
           in ((newAcc, current), newSeeds)

checkMissingDepsCauldron ::
  -- | accums
  Set TypeRep ->
  -- | available at this level
  Set TypeRep ->
  Cauldron m ->
  Either MissingDependencies ()
checkMissingDepsCauldron accums available cauldron =
  Data.Foldable.for_ (demandsByConstructorsInCauldron cauldron) \(stack, tr, demanded) ->
    let missing = Set.filter (`Set.notMember` (available `Set.union` accums)) demanded
     in if Set.null missing
          then Right ()
          else Left $ MissingDependencies stack tr missing

demandsByConstructorsInCauldron :: Cauldron m -> [(CallStack, TypeRep, Set TypeRep)]
demandsByConstructorsInCauldron Cauldron {recipes} = do
  (tr, SomeRecipe _ (Recipe {bean, decos})) <- Map.toList recipes
  ( let ConstructorReps {argReps = beanArgReps} = constructorReps bean
     in [(getConstructorCallStack bean, tr, beanArgReps)]
    )
    ++ do
      decoCon <- Data.Foldable.toList decos
      let ConstructorReps {argReps = decoArgReps} = constructorReps decoCon
       in [(getConstructorCallStack decoCon, tr, decoArgReps)]

newtype DependencyCycle = DependencyCycle (NonEmpty (BeanConstructionStep, Maybe CallStack))
  deriving stock (Show)

buildPlans :: Set TypeRep -> Tree (Fire m, Cauldron m) -> Either DependencyCycle (Tree (AdjacencyMap BeanConstructionStep, (Plan, Fire m, Cauldron m)))
buildPlans secondary = traverse \(fire@Fire {shouldOmitDependency}, cauldron) -> do
  let (locations, deps) = buildDepsCauldron secondary cauldron
  -- We may omit some dependency edges to allow for cyclic dependencies.
  let graph = Graph.edges $ filter (not . shouldOmitDependency) deps
  case Graph.topSort graph of
    Left recipeCycle ->
      Left $ DependencyCycle $ recipeCycle <&> \step -> (step, Map.lookup step locations)
    Right (reverse -> plan) -> do
      let completeGraph = Graph.edges deps
      Right (completeGraph, (plan, fire, cauldron))

buildDepsCauldron :: Set TypeRep -> Cauldron m -> (Map BeanConstructionStep CallStack, [(BeanConstructionStep, BeanConstructionStep)])
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
         recipeCallStack
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
              (decoStep, decoCon) <- decos
              constructorEdges makeTargetStep decoStep (removeBeanFromArgs do constructorReps decoCon)
            full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [boiledBean]
            innerDeps =
              -- This explicit dependency between the completed bean and its
              -- "bare" undecorated form is not strictly required. It will
              -- always exist in an indirect manner, through the decorators.
              -- But it might be useful when rendering the dep graph.
              (PrimaryBean beanRep, BarePrimaryBean beanRep)
                :
                -- The chain completed bean -> decorators -> bare bean.
                zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full)
        ( Map.fromList $
            [ (bareBean, getConstructorCallStack bean),
              (boiledBean, Data.List.NonEmpty.head recipeCallStack)
            ]
              ++ do
                (decoStep, decoCon) <- decos
                [(decoStep, getConstructorCallStack decoCon)],
          beanDeps ++ decoDeps ++ innerDeps
          )

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
  let secondaryBeanReps = Cauldron.Beans.keysSet initial
   in unfoldTreeM
        ( \(initial', Node (plan, Fire {followPlanCauldron}, cauldron) rest) -> do
            newInitial' <- followPlanCauldron cauldron secondaryBeanReps initial' plan
            pure (newInitial', (,) newInitial' <$> rest)
        )
        (initial, treecipes)

followPlanStep ::
  (Monad m) =>
  (TypeRep -> Beans -> Beans) ->
  (TypeRep -> Beans -> Beans) ->
  Cauldron m ->
  Beans ->
  Beans ->
  BeanConstructionStep ->
  m Beans
followPlanStep makeBareView makeDecoView Cauldron {recipes} final super item =
  case item of
    BarePrimaryBean rep -> case fromJust do Map.lookup rep recipes of
      SomeRecipe _ (Recipe {bean = beanConstructor}) -> do
        let ConstructorReps {beanRep} = constructorReps beanConstructor
        -- We delete the beanRep before running the bean,
        -- because if we have a self-dependency, we don't want to use the bean
        -- from a previous context (if it exists) we want the bean from final.
        -- There is a test for this.
        inserter <- followConstructor beanConstructor final (makeBareView beanRep super)
        pure do inserter super
    PrimaryBeanDeco rep index -> case fromJust do Map.lookup rep recipes of
      SomeRecipe _ (Recipe {decos}) -> do
        let decoCon = decos `Data.Sequence.index` index
        let ConstructorReps {beanRep} = constructorReps decoCon
        -- Unlike before, we don't delete the beanRep before running the constructor.
        inserter <- followConstructor decoCon final (makeDecoView beanRep super)
        pure do inserter super
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
  (Monad m, Typeable bean) =>
  Constructor m bean ->
  Beans ->
  Beans ->
  m (Beans -> Beans)
followConstructor c final super = do
  (regs, bean) <- runConstructor [super, final] c
  pure \bs ->
    Cauldron.Beans.unionBeansMonoidally (getRegsReps (getConstructorArgs c)) bs regs
      & Cauldron.Beans.insert bean

-- | Sometimes the 'cook'ing process goes wrong.
data RecipeError
  = -- | The 'Cauldron' identified by 'PathToCauldron' has beans
    -- that depend on beans that can't be found either in the current 'Cauldron' or its ancestors.
    MissingDependenciesError MissingDependencies
  | -- | Beans that work both as primary beans and as secondary beans
    -- are disallowed.
    DoubleDutyBeansError DoubleDutyBeans
  | -- | Dependency cycles are disallowed by some 'Fire's.
    DependencyCycleError DependencyCycle
  deriving stock (Show)

prettyRecipeError :: RecipeError -> String
prettyRecipeError = Data.List.intercalate "\n" . prettyRecipeErrorLines

prettyRecipeErrorLines :: RecipeError -> [String]
prettyRecipeErrorLines = \case
  MissingDependenciesError
    (MissingDependencies constructorCallStack constructorResultRep missingDependenciesReps) ->
      [ "This constructor for a value of type "
          ++ show constructorResultRep
          ++ ":"
      ]
        ++ (("\t" ++) <$> prettyCallStackLines constructorCallStack)
        ++ [ "is missing the following dependencies:"
           ]
        ++ do
          rep <- Data.Foldable.toList missingDependenciesReps
          ["- " ++ show rep]
  DoubleDutyBeansError (DoubleDutyBeans doubleDutyMap) ->
    [ "The following beans work both as primary beans and secondary beans:"
    ]
      ++ ( flip Map.foldMapWithKey doubleDutyMap \rep (secCS, primCS) ->
             [ "- " ++ show rep ++ " is a secondary bean in this constructor:"
             ]
               ++ (("\t" ++) <$> prettyCallStackLines secCS)
               ++ [ "  and a primary bean in this recipe:"
                  ]
               ++ (("\t" ++) <$> prettyCallStackLines primCS)
         )
  DependencyCycleError (DependencyCycle theCycle) ->
    [ "Forbidden dependency cycle between bean construction steps:"
    ]
      ++ ( flip foldMap theCycle \(step, mstack) ->
             [ "- " ++ case step of
                 BarePrimaryBean rep -> "Bare bean " ++ show rep
                 PrimaryBeanDeco rep i -> "Decorator " ++ show i ++ " for bean " ++ show rep
                 PrimaryBean rep -> "Complete bean " ++ show rep
                 SecondaryBean rep -> "Secondary bean " ++ show rep
             ]
               ++ case mstack of
                 Nothing -> []
                 Just stack -> (("\t" ++) <$> prettyCallStackLines stack)
         )

-- | An edge means that the source depends on the target.
--
-- The dependencies of each bean are given separatedly from its decorators.
newtype DependencyGraph = DependencyGraph {graph :: AdjacencyMap BeanConstructionStep}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

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
        SecondaryBean rep -> p rep <> Data.Text.pack "#reg"

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
-- >>> :set -XOverloadedLists
-- >>> :set -Wno-incomplete-uni-patterns
-- >>> import Data.Functor.Identity
-- >>> import Data.Function ((&))
-- >>> import Data.Monoid

data Constructor m a = Constructor CallStack (Args (m (Regs a)))
  deriving stock (Functor)

val :: (Applicative m, Registrable nested bean, HasCallStack) => Args nested -> Constructor m bean
val x = withFrozenCallStack (val' $ fmap runIdentity $ register $ fmap Identity x)

val' :: (Applicative m, HasCallStack) => Args (Regs bean) -> Constructor m bean
val' x = Constructor callStack $ fmap pure x

val_ :: (Applicative m, HasCallStack) => Args bean -> Constructor m bean
val_ x = Constructor callStack $ fmap (pure . pure) x

eff :: (Monad m, Registrable nested bean, HasCallStack) => Args (m nested) -> Constructor m bean
eff x = withFrozenCallStack (eff' $ register x)

eff' :: (HasCallStack) => Args (m (Regs bean)) -> Constructor m bean
eff' = Constructor callStack

eff_ :: (Functor m, HasCallStack) => Args (m bean) -> Constructor m bean
eff_ x = Constructor callStack $ fmap (fmap pure) x

runConstructor :: (Monad m) => [Beans] -> Constructor m bean -> m (Beans, bean)
runConstructor bss (Constructor _ args) = do
  regs <- runArgs args (Data.Foldable.asum (taste <$> bss))
  pure (runRegs regs (getRegsReps args))

-- | Change the monad in which the 'Constructor'\'s effects take place.
hoistConstructor :: (forall x. m x -> n x) -> Constructor m bean -> Constructor n bean
hoistConstructor f (Constructor theStack theArgs) = Constructor theStack do fmap f theArgs

getConstructorArgs :: Constructor m bean -> Args (m (Regs bean))
getConstructorArgs (Constructor _ theArgs) = theArgs

getConstructorCallStack :: Constructor m bean -> CallStack
getConstructorCallStack (Constructor stack _) = stack

getRecipeCallStacks :: SomeRecipe m -> NonEmpty CallStack
getRecipeCallStacks (SomeRecipe stacks _) = stacks

keysSet :: Cauldron m -> Set TypeRep
keysSet Cauldron {recipes} = Map.keysSet recipes

restrictKeys :: Cauldron m -> Set TypeRep -> Cauldron m
restrictKeys Cauldron {recipes} trs = Cauldron {recipes = Map.restrictKeys recipes trs}
