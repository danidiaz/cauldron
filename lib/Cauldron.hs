{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Cauldron
  ( Cauldron,
    empty,
    insert,
    decorate,
    delete,
    boil,
    Mishap (..),
    BeanGraph (..),
    taste,
    exportToDot,
    --
    Args,
    args0,
    argsN,
    Regs,
    regs0,
    constructor,
    constructor0,
    constructor1,
    constructor2,
    -- * Re-exports
    Endo (..),
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
import Data.Functor.Identity
import Data.Kind
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
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
import Data.Typeable
import Multicurryable
import qualified Data.List.NonEmpty
import Data.Functor ((<&>))
import Data.Type.Equality (testEquality)
import qualified Type.Reflection

newtype Cauldron = Cauldron {recipes :: Map TypeRep (SomeRecipe_ Maybe)}

empty :: Cauldron
empty = Cauldron {recipes = Map.empty}

-- | Put a recipe (constructor) into the 'Cauldron'.
insert ::
  forall (args :: [Type]) (accums :: [Type]) (bean :: Type).
  (All Typeable args, All (Typeable `And` Monoid) accums, Typeable bean) =>
  -- | A curried function that takes the @args@ and returns the @bean@
  Args args (Regs accums bean) ->
  Cauldron ->
  Cauldron
insert con Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
      beanConF = Just do Constructor @args @accums @bean con
  Cauldron
    { recipes =
        Map.alter
          do
            \case
              Nothing ->
                Just (SomeRecipe (Recipe
                    { beanConF,
                      decoCons = Seq.empty
                    }))
              Just (SomeRecipe (r :: Recipe_ Maybe a)) ->
                case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                  Nothing -> error "should never happen"
                  Just Refl -> Just do SomeRecipe r {beanConF}
          rep
          recipes
    }

decorate_ ::
  forall (args :: [Type]) (accums :: [Type]) (bean :: Type).
  (All Typeable args, All (Typeable `And` Monoid) accums, Typeable bean) =>
  -- | Where to add the decorator is left to the caller to decide.
  (forall a. a -> Seq a -> Seq a) ->
  Args args (Regs accums (Endo bean)) ->
  Cauldron ->
  Cauldron
decorate_ addToDecos con Cauldron {recipes} = do
  let rep = typeRep (Proxy @bean)
      decoCon = Constructor @args @accums @(Endo bean) con
  Cauldron
    { recipes =
        Map.alter
          do
            \case
              Nothing ->
                Just
                  (SomeRecipe Recipe
                    { beanConF = Nothing,
                      decoCons = Seq.singleton decoCon
                    })
              Just (SomeRecipe (r :: Recipe_ Maybe a)) -> do
                case testEquality (Type.Reflection.typeRep @bean) (Type.Reflection.typeRep @a) of
                  Nothing -> error "should never happen"
                  Just Refl -> do
                    let Recipe {decoCons} = r
                    Just do SomeRecipe r {decoCons = addToDecos decoCon decoCons}
          rep
          recipes
    }

decorate ::
  forall (args :: [Type]) (accums :: [Type]) (bean :: Type).
  (All Typeable args, All (Typeable `And` Monoid) accums, Typeable bean) =>
  Args args (Regs accums (Endo bean)) ->
  Cauldron ->
  Cauldron
decorate = decorate_ do flip (Seq.|>)

delete ::
  (Typeable bean) =>
  Proxy bean ->
  Cauldron ->
  Cauldron
delete proxy Cauldron {recipes} =
  Cauldron {recipes = Map.delete (typeRep proxy) recipes}

data SomeRecipe_ f where
  SomeRecipe :: Typeable a => Recipe_ f a -> SomeRecipe_ f

type SomeRecipe = SomeRecipe_ Identity

data Recipe_ f bean where
  Recipe ::
    { beanConF :: f (Constructor bean),
      decoCons :: Seq (Constructor (Endo bean))
    } ->
    Recipe_ f bean

data Constructor component where
  Constructor ::
    (All Typeable args, All (Typeable `And` Monoid) accums) =>
    { constructor_ :: Args args (Regs accums component)
    } ->
    Constructor component

data ConstructorReps = ConstructorReps
  { argReps :: [TypeRep],
    regReps :: [TypeRep]
  }



-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330
constructorReps :: Typeable component => Constructor component -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: Args args (Regs accums component))} =
  ConstructorReps
    { argReps =
        collapse_NP do
          cpure_NP @_ @args
            do Proxy @Typeable
            typeRepHelper,
      regReps =
        collapse_NP do
          cpure_NP @_ @accums
            do Proxy @(Typeable `And` Monoid)
            typeRepHelper
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K do typeRep (Proxy @a)


constructorEdges :: Typeable component => PlanItem -> Constructor component -> [(PlanItem,PlanItem)]
constructorEdges item (constructorReps -> ConstructorReps {argReps, regReps}) = 
  -- consumers depend on their args
  (do
    argRep <- argReps
    let argItem = BuiltBean argRep 
    [(item, argItem)])
  ++
  -- regs depend on their producers
  (do
    regRep <- regReps
    let repItem = BuiltBean regRep 
    [(repItem, item)])

type Plan = [PlanItem]

data PlanItem = 
    BareBean TypeRep
  | BeanDecorator TypeRep Integer
  | BuiltBean TypeRep
  deriving stock (Show, Eq, Ord)

-- | Try to build a @bean@ from the recipes stored in the 'Cauldron'.
boil ::
  Cauldron ->
  Either Mishap (BeanGraph, Map TypeRep Dynamic)
boil Cauldron {recipes} = do
  recipes' <- first BeanlessDecorator do checkBeanlessDecos recipes
  accumSet <- first DoubleDutyBeans do checkNoRegBeans recipes'
  () <- first MissingDependencies do checkMissingDeps accumSet recipes'
  (beanGraph, plan) <- first DependencyCycle do checkCycles recipes'
  let beans = build recipes' plan
  Right (BeanGraph {beanGraph}, beans)

checkBeanlessDecos ::
  Map TypeRep (SomeRecipe_ Maybe) ->
  Either (Set TypeRep) (Map TypeRep SomeRecipe)
checkBeanlessDecos recipes =
  case flip
    Map.foldMapWithKey
    recipes
    do
      \beanRep -> \case
        SomeRecipe (recipe@Recipe {beanConF = Just con}) -> 
          (Set.empty, 
           Map.singleton beanRep (SomeRecipe (recipe {beanConF = Identity con})))
        _ -> (Set.singleton beanRep, Map.empty) of
    (missing, _)
      | not do Data.List.null missing ->
          Left missing
    (_, result) ->
      Right result

checkNoRegBeans ::
  Map TypeRep SomeRecipe ->
  Either (Set TypeRep) (Set TypeRep)
checkNoRegBeans recipes = do
  let common = Set.intersection accumSet (Map.keysSet recipes)
  if not (Set.null common)
    then Left common
    else Right accumSet 
  where
    accumSet = Set.fromList do
      recipe <- Data.Foldable.toList recipes
      case recipe of 
        (SomeRecipe Recipe { beanConF = Identity beanCon, decoCons}) -> do
          let ConstructorReps { regReps = beanAccums } = constructorReps beanCon
          beanAccums ++ do 
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps { regReps = decoAccums } = constructorReps decoCon
            decoAccums

-- | TODO: handle decorator and accum dependencies as well.
-- TODO: reg dependencies should never count as missing, because they can
-- be trivially produced. 
-- However, they should be present in the plan for the sake of ordering.
checkMissingDeps ::
  Set TypeRep ->
  Map TypeRep SomeRecipe ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDeps accumSet recipes = do
  let demandedMap = Set.filter (`Map.notMember` recipes) . demanded <$> recipes
  if Data.Foldable.any (not . Set.null) demandedMap 
    then Left demandedMap
    else Right ()
  where 
    demanded :: SomeRecipe -> Set TypeRep
    demanded (SomeRecipe Recipe { beanConF = Identity beanCon, decoCons}) = (Set.fromList do 
          let ConstructorReps { argReps = beanArgReps } = constructorReps beanCon
          beanArgReps ++ do
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps { argReps = decoArgReps } = constructorReps decoCon
            decoArgReps) `Set.difference` accumSet




checkCycles ::
  Map TypeRep SomeRecipe ->
  Either (Graph.Cycle PlanItem) (AdjacencyMap PlanItem, Plan)
checkCycles recipes = do
  let beanGraph =
        Graph.edges
          do
            flip
              Map.foldMapWithKey
              recipes
              \beanRep (SomeRecipe (Recipe { 
                  beanConF = Identity beanCon,
                  decoCons
                })) -> do
                let bareBean = BareBean beanRep
                    builtBean = BuiltBean beanRep
                    decos = do 
                      (decoIndex, decoCon) <- zip [1 :: Integer ..] (Data.Foldable.toList decoCons) 
                      [(BeanDecorator beanRep decoIndex, decoCon)]
                    beanDeps = constructorEdges bareBean beanCon
                    decoDeps = concatMap (uncurry constructorEdges) decos
                    full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [builtBean]
                    innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full) 
                beanDeps ++ decoDeps ++ innerDeps
  case Graph.topSort beanGraph of
    Left recipeCycle ->
      Left recipeCycle
    Right plan -> Right (beanGraph, plan)

build ::
  Map TypeRep SomeRecipe ->
  Plan ->
  Map TypeRep Dynamic
build recipes =
  Data.List.foldl'
    do
      \dynMap -> \case
          BareBean rep -> case fromJust do Map.lookup rep recipes of
            SomeRecipe (Recipe { beanConF = Identity beanCon }) -> do
              let dyn = toDyn do followConstructor dynMap beanCon
              Map.insert (dynTypeRep dyn) dyn dynMap
          BuiltBean _ -> dynMap
          BeanDecorator rep index -> case fromJust do Map.lookup rep recipes of
            SomeRecipe (Recipe { decoCons }) -> do
                let indexStartingAt0 = fromIntegral (pred index)
                    decoCon = fromJust do Seq.lookup indexStartingAt0 decoCons
                    dyn = toDyn do followDecorator dynMap decoCon
                Map.insert (dynTypeRep dyn) dyn dynMap
    Map.empty

data Mishap
  = BeanlessDecorator (Set TypeRep)
  -- | Beans working as accumulartors and regular beans.
  | DoubleDutyBeans (Set TypeRep)
  | MissingDependencies (Map TypeRep (Set TypeRep))
  | DependencyCycle (NonEmpty PlanItem)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap PlanItem}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor :: Map TypeRep Dynamic -> Constructor component -> component
followConstructor theDyns Constructor {constructor_} = do
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor theDyns
      (_, bean) = runRegs do runArgs constructor_ args
  bean

followDecorator :: 
    forall component . Typeable component => 
    Map TypeRep Dynamic -> 
    Constructor (Endo component) -> 
    component
followDecorator theDyns decoCon  = do
  let Endo deco = followConstructor theDyns decoCon
      baseDyn = fromJust do Map.lookup (typeRep (Proxy @component)) theDyns
      base = fromJust do fromDynamic baseDyn
  deco base

newtype Extractor a = Extractor {runExtractor :: Map TypeRep Dynamic -> a}
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let runExtractor dyns =
        fromJust do taste (Proxy @a) dyns
   in Extractor {runExtractor}

exportToDot :: FilePath -> BeanGraph -> IO ()
exportToDot filepath BeanGraph {beanGraph} = do
  let prettyRep = 
        let p rep = Data.Text.pack do tyConName do typeRepTyCon rep
        in
        \case
          BareBean rep -> p rep <>  Data.Text.pack "#0"
          BeanDecorator rep index -> p rep <> Data.Text.pack ("#" ++ show index)
          BuiltBean rep -> p rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          beanGraph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)

taste :: forall a. (Typeable a) => Proxy a -> Map TypeRep Dynamic -> Maybe a
taste _ dyns = do
  let rep = typeRep (Proxy @a)
  dyn <- Map.lookup rep dyns
  fromDynamic @a dyn

newtype Args args r = Args {runArgs :: NP I args -> r}
  deriving newtype (Functor, Applicative, Monad)

args0 :: r -> Args '[] r
args0 r = Args do \_ -> r

argsN ::
  forall (args :: [Type]) r curried.
  (MulticurryableF args r curried (IsFunction curried)) =>
  curried ->
  Args args r
argsN = Args . multiuncurry

newtype Regs (regs :: [Type]) r = Regs {runRegs :: (NP I regs, r)}
  deriving newtype (Functor)

regs0 :: r -> Regs '[] r
regs0 r = Regs (Nil, r)

constructor :: 
  forall r (args :: [Type]) curried.
  (MulticurryableF args r curried (IsFunction curried)) =>
  curried ->
  Args args (Regs '[] r)
constructor = constructor0

constructor0 :: 
  forall r (args :: [Type]) curried.
  (MulticurryableF args r curried (IsFunction curried)) =>
  curried ->
  Args args (Regs '[] r)
constructor0 curried =
  regs0 <$> argsN curried 

constructor1 :: 
  forall r reg (args :: [Type]) curried.
  (MulticurryableF args (reg, r) curried (IsFunction curried)) =>
  curried ->
  Args args (Regs '[reg] r)
constructor1 curried =
  argsN curried <&> \(reg, r) -> Regs ((I reg :* Nil), r) 

constructor2 :: 
  forall r reg1 reg2 (args :: [Type]) curried.
  (MulticurryableF args (reg1, reg2, r) curried (IsFunction curried)) =>
  curried ->
  Args args (Regs '[reg1, reg2] r)
constructor2 curried =
  argsN curried <&> \(reg1, reg2, r) -> Regs ((I reg1 :* I reg2 :* Nil), r) 