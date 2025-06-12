{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
-- These extensions, while not required, play well with the library:
--
-- @
-- {-# LANGUAGE ApplicativeDo #-} -- For building complex values in the Args applicative.
-- {-# LANGUAGE OverloadedLists #-} -- For avoiding explicit calls to fromRecipeList and fromDecoList
-- @
--
-- An example of using a 'Cauldron' to wire the constructors of dummy @A@, @B@, @C@ datatypes:
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
--           recipe @A $ val $ wire makeA,
--           recipe @B $ val $ wire makeB,
--           recipe @C $ eff $ wire makeC -- we use eff because the constructor has IO effects
--         ]
--   action <- either throwIO pure $ cook forbidDepCycles cauldron
--   beans <- action
--   pure $ taste @C beans
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
    fromRecipeList,
    toRecipeMap,
    hoistCauldron,
    hoistCauldron',

    -- * Recipes
    Recipe (..),
    ToRecipe,
    fromDecoList,
    (Data.Sequence.|>),
    (Data.Sequence.<|),
    hoistRecipe,
    hoistRecipe',

    -- ** How decorators work
    -- $decos

    -- ** Hiding a 'Recipe''s bean type
    SomeRecipe,
    recipe,
    withRecipe,
    getRecipeCallStack,

    -- * Constructors
    -- $constructors
    Constructor,
    val_,
    val,
    val',
    eff_,
    ioEff_,
    eff,
    ioEff,
    eff',
    wire,
    getConstructorArgs,
    getConstructorCallStack,
    hoistConstructor,
    hoistConstructor',

    -- ** Registering secondary beans
    -- $secondarybeans

    -- * Cooking the beans
    cook,

    -- ** How loopy can we get?
    Fire,
    forbidDepCycles,
    allowSelfDeps,
    allowDepCycles,

    -- ** Tasting the results
    Beans,
    taste,

    -- ** When things go wrong
    RecipeError (..),
    MissingDependencies (..),
    DoubleDutyBeans (..),
    DependencyCycle (..),
    prettyRecipeError,
    prettyRecipeErrorLines,

    -- ** Visualizing dependencies between beans.
    getDependencyGraph,
    DependencyGraph,
    writeAsDot,
    defaultStyle,
    setVertexName,
    BeanConstructionStep (..),
    toAdjacencyMap,

    -- *** Simplifying the dep graph
    -- $simplifygraph
    removeSecondaryBeans,
    removeDecos,
    collapseToPrimaryBeans,
  )
where

import Cauldron.Graph (AdjacencyMap)
import Cauldron.Graph qualified as Graph
import Cauldron.Graph.Algorithm qualified as Graph
import Cauldron.Graph.Export.Dot qualified as Dot
import Cauldron.Args
import Cauldron.Beans (SomeMonoidTypeRep (..))
import Cauldron.Beans qualified
import Control.Exception (Exception (..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifunctor (first)
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
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exception (CallStack, prettyCallStackLines)
import GHC.IsList
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Type.Reflection qualified
import Data.String (IsString(..))
import System.IO qualified

-- | A map of bean recipes, indexed by the 'TypeRep' of the bean each recipe
-- ultimately produces. Only one recipe is allowed for each bean type.
-- Parameterized by the monad @m@ in which the recipe 'Constructor's might have
-- effects.
type Cauldron :: (Type -> Type) -> Type
newtype Cauldron m where
  Cauldron :: {recipeMap :: Map TypeRep (SomeRecipe m)} -> Cauldron m

empty :: Cauldron m
empty = Cauldron Map.empty

-- | Union of two 'Cauldron's, right-biased: prefers 'Recipe's from the /right/ cauldron when
-- both contain the same key. (Note that 'Data.Map.Map' is left-biased.)
instance Semigroup (Cauldron m) where
  Cauldron {recipeMap = r1} <> Cauldron {recipeMap = r2} = Cauldron do Map.unionWith (flip const) r1 r2

instance Monoid (Cauldron m) where
  mempty = Cauldron Map.empty

instance IsList (Cauldron m) where
  type Item (Cauldron m) = SomeRecipe m
  toList (Cauldron {recipeMap}) = Map.elems recipeMap
  fromList = fromRecipeList

-- | Change the monad used by the 'Recipe's in the 'Cauldron'.
hoistCauldron :: (forall x. m x -> n x) -> Cauldron m -> Cauldron n
hoistCauldron f (Cauldron {recipeMap}) = Cauldron {recipeMap = hoistSomeRecipe f <$> recipeMap}

-- | More general form of 'hoistCauldron' that lets you modify the 'Args'
-- inside all the 'Recipe's in the 'Cauldron'. See 'hoistRecipe''.
hoistCauldron' ::
  -- | Transformation to apply to the base constructor of each recipe.
  (forall x. (Typeable x) => Args (m (Regs x)) -> Args (n (Regs x))) ->
  -- | Transformation to apply to each decorator. Takes the decorator index as parameter.
  (forall x. (Typeable x) => Int -> Args (m (Regs x)) -> Args (n (Regs x))) ->
  Cauldron m ->
  Cauldron n
hoistCauldron' f fds Cauldron {recipeMap} =
  Cauldron
    { recipeMap = Map.map (hoistSomeRecipe' f fds) recipeMap
    }

-- | In order to put recipes producing different bean types into a container, we
-- need to hide each recipe's bean type. This wrapper allows that.
type SomeRecipe :: (Type -> Type) -> Type
data SomeRecipe m where
  SomeRecipe :: (Typeable bean) => {_recipeCallStack :: CallStack, _recipe :: Recipe m bean} -> SomeRecipe m

-- | Build a 'SomeRecipe' from a 'Recipe' or a 'Constructor'. See 'ToRecipe'.
--
-- Useful in combination with 'fromRecipeList'.
recipe ::
  forall {recipelike} {m} bean.
  (ToRecipe recipelike, Typeable bean, HasCallStack) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  SomeRecipe m
recipe theRecipe = withFrozenCallStack do
  SomeRecipe callStack (toRecipe theRecipe)

-- | Access the 'Recipe' inside a 'SomeRecipe'.
withRecipe :: forall {m} r. (forall bean. (Typeable bean) => Recipe m bean -> r) -> SomeRecipe m -> r
withRecipe f (SomeRecipe {_recipe}) = f _recipe

getRecipeRep :: SomeRecipe m -> TypeRep
getRecipeRep = withRecipe go
  where
    go :: forall bean m. (Typeable bean) => Recipe m bean -> TypeRep
    go _ = typeRep (Proxy @bean)

fromRecipeList :: [SomeRecipe m] -> Cauldron m
fromRecipeList =
  foldMap \sr -> Cauldron {recipeMap = Map.singleton (getRecipeRep sr) sr}

toRecipeMap :: Cauldron m -> Map TypeRep (SomeRecipe m)
toRecipeMap Cauldron {recipeMap} = recipeMap

hoistSomeRecipe :: (forall x. m x -> n x) -> SomeRecipe m -> SomeRecipe n
hoistSomeRecipe f r@SomeRecipe {_recipe} = r {_recipe = hoistRecipe f _recipe}

hoistSomeRecipe' ::
  forall m n.
  (forall x. (Typeable x) => Args (m (Regs x)) -> Args (n (Regs x))) ->
  (forall x. (Typeable x) => Int -> Args (m (Regs x)) -> Args (n (Regs x))) ->
  SomeRecipe m ->
  SomeRecipe n
hoistSomeRecipe' f fds sr = withRecipe go sr
  where
    go :: forall bean. (Typeable bean) => Recipe m bean -> SomeRecipe n
    go r = sr {_recipe = hoistRecipe' (f @bean) (fds @bean) r}

-- | Instructions for how to build a value of type @bean@ while possibly
-- performing actions in the monad @m@.
--
-- Because the instructions aren't really run until the 'Cauldron' is 'cook'ed,
-- they can be modified with functions like 'adjust', in order to change the
-- base bean 'Constructor', or add or remove decorators.
type Recipe :: (Type -> Type) -> Type -> Type
data Recipe m bean = Recipe
  { -- | How to build the bean itself.
    bean :: Constructor m bean,
    -- | A 'Data.Sequence.Sequence' of decorators that will wrap the bean. There might be no decorators.
    --
    -- See 'fromDecoList', 'Data.Sequence.|>' and 'Data.Sequence.<|'.
    decos :: Seq (Constructor m bean)
  }

fromDecoList :: [Constructor m bean] -> Seq (Constructor m bean)
fromDecoList = Data.Sequence.fromList

-- | Convenience typeclass that allows passing either 'Recipe's or 'Constructor's
-- to the 'insert' and 'recipe' functions.
type ToRecipe :: ((Type -> Type) -> Type -> Type) -> Constraint
class ToRecipe recipelike where
  toRecipe :: recipelike m bean -> Recipe m bean

-- | Simply identity.
instance ToRecipe Recipe where
  toRecipe = id

-- | 'Constructor' is converted to a 'Recipe' without decorators.
instance ToRecipe Constructor where
  toRecipe bean = Recipe {bean, decos = Data.Sequence.empty}

-- | Change the monad used by the bean\'s main 'Constructor' and its decos.
hoistRecipe :: (forall x. m x -> n x) -> Recipe m bean -> Recipe n bean
hoistRecipe f (Recipe {bean, decos}) =
  Recipe
    { bean = hoistConstructor f bean,
      decos = hoistConstructor f <$> decos
    }

-- | More general form of 'hoistRecipe' that enables precise control over the inner `Args`
-- of each constructor in the 'Recipe'.
hoistRecipe' ::
  -- | Transformation to apply to the base constructor.
  (Args (m (Regs bean)) -> Args (n (Regs bean))) ->
  -- | Transformation to apply to each decorator. Takes the decorator index as parameter.
  (Int -> Args (m (Regs bean)) -> Args (n (Regs bean))) ->
  Recipe m bean ->
  Recipe n bean
hoistRecipe' f fds (Recipe {bean, decos}) =
  Recipe
    { bean = hoistConstructor' f bean,
      decos = Data.Sequence.mapWithIndex (\i deco -> hoistConstructor' (fds i) deco) decos
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
-- add behaviors like caching and logging to the functions.
--
-- The order of the decorators in the sequence is the order in which they modify
-- the underlying bean. First decorator wraps first, last decorator wraps last.
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
--             bean = val $ wire makeFoo,
--             decos = [
--                  val $ wire makeFooDeco1,
--                  eff $ wire makeFooDeco2
--               ]
--           }
--         ]
--   action <- either throwIO pure $ cook forbidDepCycles cauldron
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
-- Bean-producing and bean-decorating functions need to be coaxed into 'Constructor's in order to be used in 'Cauldron's.

data ConstructorReps where
  ConstructorReps ::
    { beanRep :: TypeRep,
      argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

-- | Put a 'Recipe' into the 'Cauldron'.
--
-- Only one recipe is allowed for each bean type, so 'insert' for a
-- bean will overwrite any previous recipe for that bean.
insert ::
  forall {recipelike} {m} (bean :: Type).
  (Typeable bean, ToRecipe recipelike, HasCallStack) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  Cauldron m ->
  Cauldron m
insert recipelike Cauldron {recipeMap} = withFrozenCallStack do
  let rep = typeRep (Proxy @bean)
  Cauldron {recipeMap = Map.insert rep (SomeRecipe callStack (toRecipe recipelike)) recipeMap}

-- | Tweak a 'Recipe' inside the 'Cauldron', if the recipe exists.
adjust ::
  forall {m} bean.
  (Typeable bean) =>
  (Recipe m bean -> Recipe m bean) ->
  Cauldron m ->
  Cauldron m
adjust f (Cauldron {recipeMap}) = withFrozenCallStack do
  let rep = typeRep (Proxy @bean)
  Cauldron
    { recipeMap =
        recipeMap
          & Map.adjust
            do
              \r@SomeRecipe {_recipe = _recipe :: Recipe m a} ->
                case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                  Nothing -> error "should never happen"
                  Just Refl -> r {_recipe = f _recipe}
            rep
    }

delete ::
  forall m.
  TypeRep ->
  Cauldron m ->
  Cauldron m
delete tr Cauldron {recipeMap} =
  Cauldron {recipeMap = Map.delete tr recipeMap}

-- | Strategy for dealing with dependency cycles.
--
-- (The name is admittedly uninformative; the culinary metaphor was stretched too far.)
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
-- argument comes \"from the future\" and is already decorated.
--
-- Note that a 'MonadFix' instance is required of the initialization monad.
--
-- __BEWARE__: Pattern-matching too eagerly on a \"bean from the future\" during
-- construction will cause infinite loops or, if you are lucky, throw
-- 'Control.Exception.FixIOException's.
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

-- | Allow /any/ kind of dependency cycles.
--
-- Usually comes in handy for creating serializers / deserializers for mutually
-- dependent types.
--
-- Note that a 'MonadFix' instance is required of the initialization monad.
--
-- __BEWARE__: Pattern-matching too eagerly on argument beans during
-- construction will cause infinite loops or, if you are lucky, throw
-- 'Control.Exception.FixIOException's.
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
constructorReps :: forall {m} bean. (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps (getConstructorArgs -> c) =
  ConstructorReps
    { beanRep = typeRep (Proxy @bean),
      argReps = getArgsReps c,
      regReps =
        c
          & getRegsReps
          & Set.map (\mtr@(SomeMonoidTypeRep tr) -> Data.Semigroup.Arg (Type.Reflection.SomeTypeRep tr) (toDyn (Cauldron.Beans.someMonoidTypeRepMempty mtr)))
          & Map.fromArgSet
    }

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

-- | Build the beans using the recipeMap stored in the 'Cauldron'.
--
-- Any secondary beans that are registered by constructors are aggregated
-- monoidally.
cook ::
  forall m.
  (Monad m) =>
  Fire m ->
  Cauldron m ->
  Either RecipeError (m Beans)
cook fire cauldron = do
  accumMap <- first DoubleDutyBeansError do checkNoDoubleDutyBeans cauldron
  () <- first MissingDependenciesError do checkMissingDeps (Map.keysSet accumMap) (Cauldron.keysSet cauldron) cauldron
  plan <- first DependencyCycleError do buildPlan fire (Map.keysSet accumMap) cauldron
  Right $ followPlan fire cauldron (fromDynList (Data.Foldable.toList accumMap)) plan

newtype DoubleDutyBeans = DoubleDutyBeans (Map TypeRep (CallStack, CallStack))
  deriving stock (Show)

-- | Get a graph of dependencies between 'BeanConstructionStep's. The graph can
-- be obtained even if the 'Cauldron' can't be 'cook'ed successfully.
getDependencyGraph :: Cauldron m -> DependencyGraph
getDependencyGraph cauldron =
  let (accumMap, _) = cauldronRegs cauldron
      (_, deps) = buildDepsCauldron (Map.keysSet accumMap) cauldron
   in DependencyGraph {graph = Graph.edges deps}

checkNoDoubleDutyBeans ::
  Cauldron m ->
  Either DoubleDutyBeans (Map TypeRep Dynamic)
checkNoDoubleDutyBeans cauldron = do
  let (accumMap, beanSet) = cauldronRegs cauldron
  let common = Map.intersectionWith (,) (fst <$> accumMap) beanSet
  if not (Map.null common)
    then Left $ DoubleDutyBeans common
    else Right $ snd <$> accumMap

cauldronRegs :: Cauldron m -> (Map TypeRep (CallStack, Dynamic), Map TypeRep CallStack)
cauldronRegs Cauldron {recipeMap} =
  Map.foldMapWithKey
    do \rep aRecipe -> (recipeRegs aRecipe, Map.singleton rep (getRecipeCallStack aRecipe))
    recipeMap

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
  -- | available at this level
  Set TypeRep ->
  Cauldron m ->
  Either MissingDependencies ()
checkMissingDeps accums available cauldron =
  Data.Foldable.for_ (demandsByConstructorsInCauldron cauldron) \(stack, tr, demanded) ->
    let missing = Set.filter (`Set.notMember` (available `Set.union` accums)) demanded
     in if Set.null missing
          then Right ()
          else Left $ MissingDependencies stack tr missing

demandsByConstructorsInCauldron :: Cauldron m -> [(CallStack, TypeRep, Set TypeRep)]
demandsByConstructorsInCauldron Cauldron {recipeMap} = do
  (tr, SomeRecipe _ (Recipe {bean, decos})) <- Map.toList recipeMap
  ( let ConstructorReps {argReps = beanArgReps} = constructorReps bean
     in [(getConstructorCallStack bean, tr, beanArgReps)]
    )
    ++ do
      decoCon <- Data.Foldable.toList decos
      let ConstructorReps {argReps = decoArgReps} = constructorReps decoCon
       in [(getConstructorCallStack decoCon, tr, decoArgReps)]

newtype DependencyCycle = DependencyCycle (NonEmpty (BeanConstructionStep, Maybe CallStack))
  deriving stock (Show)


buildPlan :: Fire m -> Set TypeRep -> Cauldron m -> Either DependencyCycle Plan
buildPlan Fire {shouldOmitDependency} secondary cauldron = do
  let (locations, deps) = buildDepsCauldron secondary cauldron
  -- We may omit some dependency edges to allow for cyclic dependencies.
  let graph = Graph.edges $ filter (not . shouldOmitDependency) deps
  case Graph.reverseTopSort graph of
    Left recipeCycle ->
      Left $ DependencyCycle $ recipeCycle <&> \step -> (step, Map.lookup step locations)
    Right plan -> do
      Right plan


-- buildPlans :: Set TypeRep -> Tree (Fire m, Cauldron m) -> Either DependencyCycle (Tree (Plan, Fire m, Cauldron m))
-- buildPlans secondary = traverse \(fire@Fire {shouldOmitDependency}, cauldron) -> do
--   let (locations, deps) = buildDepsCauldron secondary cauldron
--   -- We may omit some dependency edges to allow for cyclic dependencies.
--   let graph = Graph.edges $ filter (not . shouldOmitDependency) deps
--   case Graph.reverseTopSort graph of
--     Left recipeCycle ->
--       Left $ DependencyCycle $ recipeCycle <&> \step -> (step, Map.lookup step locations)
--     Right plan -> do
--       Right (plan, fire, cauldron)

buildDepsCauldron :: Set TypeRep -> Cauldron m -> (Map BeanConstructionStep CallStack, [(BeanConstructionStep, BeanConstructionStep)])
buildDepsCauldron secondary Cauldron {recipeMap} = do
  -- Are we depending on a primary bean, or on a monoidally aggregated secondary bean?
  -- I wonder if we could make this more uniform, it's kind of annoying to have to make this decision here...
  let makeTargetStep :: TypeRep -> BeanConstructionStep
      makeTargetStep rep =
        if rep `Set.member` secondary
          then SecondaryBean rep
          else PrimaryBean rep
  recipeMap
    & Map.foldMapWithKey
      \beanRep
       SomeRecipe
         { _recipeCallStack,
           _recipe =
             Recipe
               { bean = bean :: Constructor m bean,
                 decos
               }
         } ->
          do
            let bareBean = BarePrimaryBean beanRep
                boiledBean = PrimaryBean beanRep
                decoSteps = do
                  (decoIndex, decoCon) <- zip [0 :: Int ..] (Data.Foldable.toList decos)
                  [(PrimaryBeanDeco beanRep decoIndex, decoCon)]
                beanDeps = do
                  constructorEdges makeTargetStep bareBean (constructorReps bean)
                decoDeps = do
                  (decoStep, decoCon) <- decoSteps
                  -- We remove the bean because from the args becase, in the
                  -- case of decos, we want to depend on the in-the-making
                  -- version of the bean, not the completed bean.
                  constructorEdges makeTargetStep decoStep (removeBeanFromArgs do constructorReps decoCon)
                innerSteps = bareBean Data.List.NonEmpty.:| (fst <$> decoSteps) ++ [boiledBean]
                innerDeps =
                  -- This explicit dependency between the completed bean and its
                  -- "bare" undecorated form is not strictly required. It will
                  -- always exist in an indirect manner, through the decorators.
                  -- But it might be useful when rendering the dep graph.
                  (PrimaryBean beanRep, BarePrimaryBean beanRep)
                    :
                    -- The dep chain of completed bean -> decorators -> bare bean.
                    zip (Data.List.NonEmpty.tail innerSteps) (Data.List.NonEmpty.toList innerSteps)
            ( Map.fromList $
                [ (bareBean, getConstructorCallStack bean),
                  (boiledBean, _recipeCallStack)
                ]
                  ++ do
                    (decoStep, decoCon) <- decoSteps
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

-- | Can we get rid of this function in favor of followPlanCaudron?
followPlan ::
  Monad m =>
  Fire m ->
  Cauldron m ->
  Beans ->
  Plan -> 
  m Beans
followPlan Fire {followPlanCauldron} cauldron initialBeans plan =
  let -- the very first initial beans are the accumulators?
      secondaryBeanReps = Cauldron.Beans.keysSet initialBeans
   in do
            currentStageBeans <- followPlanCauldron cauldron secondaryBeanReps initialBeans plan
            pure currentStageBeans

followPlanStep ::
  (Monad m) =>
  (TypeRep -> Beans -> Beans) ->
  (TypeRep -> Beans -> Beans) ->
  Cauldron m ->
  Beans ->
  Beans ->
  BeanConstructionStep ->
  m Beans
followPlanStep makeBareView makeDecoView Cauldron {recipeMap} final super item =
  case item of
    BarePrimaryBean rep -> case fromJust do Map.lookup rep recipeMap of
      SomeRecipe {_recipe = Recipe {bean}} -> do
        let ConstructorReps {beanRep} = constructorReps bean
        -- We delete the beanRep before running the bean,
        -- because if we have a self-dependency, we don't want to use the bean
        -- from a previous context (if it exists) we want the bean from final.
        -- There is a test for this.
        inserter <- followConstructor bean final (makeBareView beanRep super)
        pure do inserter super
    PrimaryBeanDeco rep index -> case fromJust do Map.lookup rep recipeMap of
      SomeRecipe {_recipe = Recipe {decos}} -> do
        let deco = decos `Data.Sequence.index` index
        let ConstructorReps {beanRep} = constructorReps deco
        -- Unlike before, we don't delete the beanRep before running the constructor.
        inserter <- followConstructor deco final (makeDecoView beanRep super)
        pure do inserter super
    -- \| We do nothing here, the work has been done in previous 'BarePrimaryBean' and
    -- 'PrimaryBeanDeco' steps.
    PrimaryBean {} -> pure super
    -- \| We do nothing here, secondary beans are built as a byproduct
    -- of primary beans and decorators.
    SecondaryBean {} -> pure super

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
  = -- | A 'Constructor' depends on beans that can't be found either in the current 'Cauldron' or its ancestors.
    MissingDependenciesError MissingDependencies
  | -- | Beans that work both as primary beans and as secondary beans
    -- are disallowed.
    DoubleDutyBeansError DoubleDutyBeans
  | -- | Dependency cycles are disallowed by some 'Fire's.
    DependencyCycleError DependencyCycle
  deriving stock (Show)

instance Exception RecipeError where
  displayException = prettyRecipeError

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

-- | Remove all vertices and edges related to secondary beans.
removeSecondaryBeans :: DependencyGraph -> DependencyGraph
removeSecondaryBeans DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case SecondaryBean {} -> False; _ -> True) graph}

-- | Remove all vertices and edges related to bean decorators.
removeDecos :: DependencyGraph -> DependencyGraph
removeDecos DependencyGraph {graph} =
  DependencyGraph {graph = Graph.induce (\case PrimaryBeanDeco {} -> False; _ -> True) graph}

-- | Unifies 'PrimaryBean's with their respective 'BarePrimaryBean's and 'PrimaryBeanDeco's.
--
-- Also removes any self-loops.
collapseToPrimaryBeans :: DependencyGraph -> DependencyGraph
collapseToPrimaryBeans DependencyGraph {graph} = do
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
writeAsDot :: Dot.Style BeanConstructionStep String -> FilePath -> DependencyGraph -> IO ()
writeAsDot style filepath DependencyGraph {graph} = do
  let dot = Dot.export style graph
  System.IO.withFile filepath System.IO.WriteMode $ \handle -> do
    System.IO.hSetEncoding handle System.IO.utf8  
    System.IO.hPutStrLn handle dot

-- | Default DOT rendering style to use with 'writeAsDot'.
-- When a 'RecipeError' exists, is highlights the problematic 'BeanConstructionStep's.
defaultStyle :: (Monoid s, IsString s) => Maybe RecipeError -> Dot.Style BeanConstructionStep s
defaultStyle merr =
  -- https://graphviz.org/docs/attr-types/style/
  -- https://hackage.haskell.org/package/algebraic-graphs-0.7/docs/Algebra-Graph-Export-Dot.html
  (Dot.defaultStyle defaultStepToText)
    { Dot.vertexAttributes = \step -> case merr of
        Nothing -> []
        Just (MissingDependenciesError (MissingDependencies _ _ missing)) ->
          case step of
            PrimaryBean rep
              | Set.member rep missing ->
                  [ fromString "style" Dot.:= fromString "dashed",
                    fromString "color" Dot.:= fromString "red"
                  ]
            _ -> []
        Just (DoubleDutyBeansError (DoubleDutyBeans (Map.keysSet -> bs))) ->
          case step of
            PrimaryBean rep
              | Set.member rep bs ->
                  [ fromString "style" Dot.:= fromString "bold",
                    fromString "color" Dot.:= fromString "green"
                  ]
            SecondaryBean rep
              | Set.member rep bs ->
                  [ fromString "style" Dot.:= fromString "bold",
                    fromString "color" Dot.:= fromString "green"
                  ]
            _ -> []
        Just (DependencyCycleError (DependencyCycle (Set.fromList . Data.Foldable.toList . fmap fst -> cycleStepSet))) ->
          if Set.member step cycleStepSet
            then
              [ fromString "style" Dot.:= fromString "bold",
                fromString "color" Dot.:= fromString "blue"
              ]
            else []
    }

-- | Change the default way of how 'BeanConstructionStep's are rendered to text.
setVertexName :: IsString s => (BeanConstructionStep -> s) -> Dot.Style BeanConstructionStep s -> Dot.Style BeanConstructionStep s
setVertexName vertexName style = style {Dot.vertexName}

defaultStepToText :: IsString s => BeanConstructionStep -> s
defaultStepToText =
  let p rep = show rep
   in \case
        BarePrimaryBean rep -> fromString $ p rep ++ "#bare"
        PrimaryBeanDeco rep index -> fromString $ p rep ++ "#deco#" ++ show index
        PrimaryBean rep -> fromString $ p rep
        SecondaryBean rep -> fromString $ p rep ++ "#agg"


-- | A way of building value of type @bean@, potentially requiring some
-- dependencies, potentially returning some secondary beans
-- along the primary @bean@ result, and also potentially requiring some
-- initialization effect in a monad @m@.
--
-- Note that only the type of the primary @bean@ is reflected in the
-- 'Constructor' type. Those of the dependencies and secondary beans are not.
--
-- A typical initialization monad will be 'IO', used for example to create
-- mutable references that the bean will use internally. Sometimes the
-- constructor will allocate resources with bracket-like operations, and in that
-- case a monad like 'Cauldron.Managed.Managed' might be needed instead.
data Constructor m bean = Constructor
  { _constructorCallStack :: CallStack,
    _args :: Args (m (Regs bean))
  }

-- | Create a 'Constructor' from an 'Args' value that returns a 'bean'.
--
-- Usually, the 'Args' value will be created by 'wire'ing a constructor function.
val_ :: forall bean m. (Applicative m, HasCallStack) => Args bean -> Constructor m bean
val_ x = Constructor callStack $ fmap (pure . pure) x

-- | Like 'val_', but examines the @nested@ value returned by the 'Args' looking
-- for (potentially nested) tuples.  All tuple components except the
-- rightmost-innermost one are registered as secondary beans (if they have
-- 'Monoid' instances, otherwise 'val' won't compile).
val :: forall {nested} bean m. (Registrable nested bean, Applicative m, HasCallStack) => Args nested -> Constructor m bean
val x = withFrozenCallStack (val' $ fmap runIdentity $ register $ fmap Identity x)

-- | Like 'val', but uses an alternative form of registering secondary beans.
-- Less 'Registrable' typeclass magic, but more verbose. Likely not what you want.
val' :: forall bean m. (Applicative m, HasCallStack) => Args (Regs bean) -> Constructor m bean
val' x = Constructor callStack $ fmap pure x

-- | Create a 'Constructor' from an 'Args' value that returns an initialization
-- effect that produces 'bean'.
--
-- Usually, the 'Args' value will be created by 'wire'ing an effectul constructor function.
eff_ :: forall bean m. (Functor m, HasCallStack) => Args (m bean) -> Constructor m bean
eff_ x = Constructor callStack $ fmap (fmap pure) x

-- | Like 'eff_', but lifts 'IO' constructor effects into a general 'MonadIO'.
ioEff_ :: forall bean m. (MonadIO m, HasCallStack) => Args (IO bean) -> Constructor m bean
ioEff_ args = withFrozenCallStack (hoistConstructor liftIO (eff_ args))

-- | Like 'eff_', but examines the @nested@ value produced by the action
-- returned by the 'Args' looking for (potentially nested) tuples.  All tuple
-- components except the rightmost-innermost one are registered as secondary
-- beans (if they have 'Monoid' instances, otherwise 'eff' won't compile).
eff :: forall {nested} bean m. (Registrable nested bean, Monad m, HasCallStack) => Args (m nested) -> Constructor m bean
eff x = withFrozenCallStack (eff' $ register x)

-- | Like 'eff', but lifts 'IO' constructor effects into a general 'MonadIO'.
ioEff :: forall {nested} bean m. (Registrable nested bean, MonadIO m, HasCallStack) => Args (IO nested) -> Constructor m bean
ioEff args = withFrozenCallStack (hoistConstructor liftIO (eff args))

-- | Like 'eff', but uses an alternative form of registering secondary beans.
-- Less 'Registrable' typeclass magic, but more verbose. Likely not what you want.
eff' :: forall bean m. (HasCallStack) => Args (m (Regs bean)) -> Constructor m bean
eff' = Constructor callStack

runConstructor :: (Monad m) => [Beans] -> Constructor m bean -> m (Beans, bean)
runConstructor bss (Constructor {_args}) = do
  regs <- _args & runArgs (Data.Foldable.asum (taste <$> bss))
  pure (runRegs (getRegsReps _args) regs)

-- | Change the monad in which the 'Constructor'\'s effects take place.
hoistConstructor :: (forall x. m x -> n x) -> Constructor m bean -> Constructor n bean
hoistConstructor f c@Constructor {_args} = c {_args = fmap f _args}

-- | More general form of 'hoistConstructor' that enables precise control over the inner `Args`.
hoistConstructor' :: (Args (m (Regs bean)) -> Args (n (Regs bean))) -> Constructor m bean -> Constructor n bean
hoistConstructor' f c@Constructor {_args} = c {_args = f _args}

-- | Get the inner 'Args' value for the 'Constructor', typically for inspecting
-- 'TypeRep's of its arguments/registrations.
getConstructorArgs :: Constructor m bean -> Args (m (Regs bean))
getConstructorArgs (Constructor {_args}) = _args

-- | For debugging purposes, 'Constructor's remember the 'CallStack'
-- of when they were created.
getConstructorCallStack :: Constructor m bean -> CallStack
getConstructorCallStack (Constructor {_constructorCallStack}) = _constructorCallStack

-- | For debugging purposes, 'SomeRecipe's remember the 'CallStack'
-- of when they were created.
getRecipeCallStack :: SomeRecipe m -> CallStack
getRecipeCallStack (SomeRecipe {_recipeCallStack}) = _recipeCallStack

-- | The set of all 'TypeRep' keys of the map.
keysSet :: Cauldron m -> Set TypeRep
keysSet Cauldron {recipeMap} = Map.keysSet recipeMap

-- | Restrict a 'Cauldron' to only those 'TypeRep's found in a 'Set'.
restrictKeys :: Cauldron m -> Set TypeRep -> Cauldron m
restrictKeys Cauldron {recipeMap} trs = Cauldron {recipeMap = Map.restrictKeys recipeMap trs}

-- $simplifygraph
--
-- 'DependencyGraph's can get complex and difficult to intepret because they
-- include bean decorators and secondary beans, details in which we many not be
-- interested.
--
-- These functions help simplify 'DependencyGraph's before passing them to
-- 'writeAsDot'. They can be composed between themselves.

-- $secondarybeans
--
-- There is an exception to the 'Cauldron' rule that each bean type can only
-- be produced by a single 'Recipe' in the 'Cauldron'.
--
-- 'Constructor's can produce, besides their \"primary\" bean result,
-- \"secondary\" beans that are not reflected in the 'Constructor' signature.
-- Multiple constructors across different 'Recipe's can produce secondary beans of the
-- same type.
--
-- Secondary beans are a bit special, in that:
--
-- * The value that is \"seen"\ by a 'Constructor' that depends on a secondary bean
--   is the aggregation of /all/ values produced for that bean in the 'Cauldron'. This
--   means that secondary beans must have 'Monoid' instances, to enable aggregation.
--
-- * When calculating build plan steps for a 'Cauldron', 'Constructor's that depend on a
--   secondary bean come after /all/ of the 'Constructor's that produce that secondary bean.
--
-- * Secondary beans can't be decorated.
--
-- * A bean type can't be primary and secondary at the same time. See 'DoubleDutyBeansError'.
--
-- What are secondary beans useful for?
--
-- * Exposing some uniform control or inspection interface for certain beans.
--
-- * Registering tasks or workers that must be run after application initialization.
--
-- The simplest way of registering secondary beans is to pass an 'Args' value returning a tuple
-- to the 'val' (for pure constructors) or 'eff' (for effectful constructors) functions. Components
-- of the tuple other than the rightmost component are considered secondary beans:
--
-- >>> :{
-- con :: Constructor Identity String
-- con = val $ pure (Sum @Int, All False, "foo")
-- effCon :: Constructor IO String
-- effCon = eff $ pure $ pure @IO (Sum @Int, All False, "foo")
-- :}
--
-- Example of how secondary bean values are accumulated:
--
-- >>> :{
-- data U = U deriving Show
-- data V = V deriving Show
-- makeU :: (Sum Int, U)
-- makeU = (Sum 1, U)
-- makeV :: U -> (Sum Int, V)
-- makeV = \_ -> (Sum 7, V)
-- newtype W = W (Sum Int) deriving Show -- depends on the secondary bean
-- :}
--
-- >>> :{
-- do
--   let cauldron :: Cauldron Identity
--       cauldron = [
--           recipe @U $ val $ wire makeU,
--           recipe @V $ val $ wire makeV,
--           recipe @W $ val $ wire W
--         ]
--   Identity beans <- either throwIO pure $ cook forbidDepCycles cauldron
--   pure $ taste @W beans
-- :}
-- Just (W (Sum {getSum = 8}))

-- $setup
-- >>> :set -XBlockArguments
-- >>> :set -XOverloadedLists
-- >>> :set -Wno-incomplete-uni-patterns
-- >>> import Data.Functor.Identity
-- >>> import Data.Function ((&))
-- >>> import Data.Monoid
-- >>> import Data.Either (either)
-- >>> import Control.Exception (throwIO)

