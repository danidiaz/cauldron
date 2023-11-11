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
    regs1,
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
import Control.Monad (guard)

newtype Cauldron = Cauldron {recipes :: Map TypeRep (SomeRecipe_ Maybe)}

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
  { argReps :: Set TypeRep,
    regReps :: Map TypeRep Dynamic
  }

newtype Extractor a = Extractor {runExtractor :: Map TypeRep Dynamic -> a}
  deriving newtype (Functor, Applicative)

empty :: Cauldron
empty = Cauldron {recipes = Map.empty}

-- | Put a recipe (constructor) into the 'Cauldron'.
insert ::
  forall (bean :: Type) (args :: [Type]) (accums :: [Type]) .
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
  forall (bean :: Type) (args :: [Type]) (accums :: [Type]) .
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
  forall (bean :: Type) (args :: [Type]) (accums :: [Type]) .
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

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330
constructorReps :: Typeable component => Constructor component -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: Args args (Regs accums component))} =
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


constructorEdges :: Typeable component => 
  (TypeRep -> Bool) ->
  PlanItem -> 
  Constructor component -> 
  [(PlanItem,PlanItem)]
constructorEdges allowArg item (constructorReps -> ConstructorReps {argReps, regReps}) = 
  -- consumers depend on their args
  (do
    argRep <- Set.toList argReps
    guard do allowArg argRep 
    let argItem = BuiltBean argRep 
    [(item, argItem)])
  ++
  -- regs depend on their producers
  (do
    (regRep, _) <- Map.toList regReps
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
  () <- first MissingDependencies do checkMissingDeps (Map.keysSet accumSet) recipes'
  (beanGraph, plan) <- first DependencyCycle do checkCycles recipes'
  let beans = followPlan recipes' accumSet plan 
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
  Either (Set TypeRep) (Map TypeRep Dynamic)
checkNoRegBeans recipes = do
  let common = Set.intersection (Map.keysSet accumSet) (Map.keysSet recipes)
  if not (Set.null common)
    then Left common
    else Right accumSet 
  where
    accumSet = Map.fromList do
      recipe <- Data.Foldable.toList recipes
      case recipe of 
        (SomeRecipe Recipe { beanConF = Identity beanCon, decoCons}) -> do
          let ConstructorReps { regReps = beanAccums } = constructorReps beanCon
          Map.toList beanAccums ++ do 
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps { regReps = decoAccums } = constructorReps decoCon
            Map.toList decoAccums

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
          Set.toList beanArgReps ++ do
            decoCon <- Data.Foldable.toList decoCons
            let ConstructorReps { argReps = decoArgReps } = constructorReps decoCon
            Set.toList decoArgReps) `Set.difference` accumSet




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
                  beanConF = Identity (beanCon :: Constructor bean),
                  decoCons
                })) -> do
                let bareBean = BareBean beanRep
                    builtBean = BuiltBean beanRep
                    decos = do 
                      (decoIndex, decoCon) <- zip [1 :: Integer ..] (Data.Foldable.toList decoCons) 
                      [(BeanDecorator beanRep decoIndex, decoCon)]
                    noEdgesForSelfLoops = (/=) do typeRep (Proxy @bean)
                    beanDeps = constructorEdges noEdgesForSelfLoops bareBean beanCon
                    decoDeps = concatMap (uncurry do constructorEdges (const True)) decos
                    full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [builtBean]
                    innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full) 
                beanDeps ++ decoDeps ++ innerDeps
  case Graph.topSort beanGraph of
    Left recipeCycle ->
      Left recipeCycle
    Right plan -> Right (beanGraph, plan)

followPlan ::
  Map TypeRep SomeRecipe ->
  Map TypeRep Dynamic ->
  Plan ->
  Map TypeRep Dynamic
followPlan recipes initial plan = do
  let final =
        Data.List.foldl' 
          do \super -> \case
                BareBean rep -> case fromJust do Map.lookup rep recipes of
                  SomeRecipe (Recipe { beanConF = Identity beanCon }) -> do
                    let (super', bean) = followConstructor beanCon final super 
                        dyn = toDyn bean
                    Map.insert (dynTypeRep dyn) dyn super'
                BuiltBean _ -> super
                BeanDecorator rep index -> case fromJust do Map.lookup rep recipes of
                  SomeRecipe (Recipe { decoCons }) -> do
                    let indexStartingAt0 = fromIntegral (pred index)
                        decoCon = fromJust do Seq.lookup indexStartingAt0 decoCons
                        (super', bean) = followDecorator decoCon final super
                        dyn = toDyn bean
                    Map.insert (dynTypeRep dyn) dyn super'
          initial
          plan
  final 

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
followConstructor :: 
    Constructor component -> 
    Map TypeRep Dynamic -> 
    Map TypeRep Dynamic -> 
    (Map TypeRep Dynamic, component)
followConstructor Constructor {constructor_} final super = do
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor final
  case runArgs constructor_ args of
    Regs regs bean -> do
      let inserters = cfoldMap_NP (Proxy @(Typeable `And` Monoid)) makeRegInserter regs
      (appEndo inserters super, bean)

followDecorator :: 
    forall component . Typeable component => 
    Constructor (Endo component) -> 
    Map TypeRep Dynamic -> 
    Map TypeRep Dynamic -> 
    (Map TypeRep Dynamic,component)
followDecorator decoCon final super = do
  let (super', Endo deco) = followConstructor decoCon final super 
      baseDyn = fromJust do Map.lookup (typeRep (Proxy @component)) super'
      base = fromJust do fromDynamic baseDyn
  (super', deco base)


makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let runExtractor dyns =
        fromJust do taste (Proxy @a) dyns
   in Extractor {runExtractor}

makeRegInserter:: forall a. ((Typeable `And` Monoid) a) => I a -> Endo (Map TypeRep Dynamic)
makeRegInserter (I a) =
  let appEndo dynMap = do
        let reg = fromJust do taste (Proxy @a) dynMap
            dyn = toDyn (reg <> a)
        Map.insert (dynTypeRep dyn) dyn dynMap
   in Endo {appEndo}

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

data Regs (regs :: [Type]) r = Regs (NP I regs) r
  deriving Functor

regs0 :: r -> Regs '[] r
regs0 r = Regs Nil r

regs1 :: reg1 -> r -> Regs '[reg1] r
regs1 reg1 r = Regs (I reg1 :* Nil) r

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
  forall r reg' reg (args :: [Type]) curried.
  (MulticurryableF args (reg', r) curried (IsFunction curried)) =>
  (reg' -> reg) ->
  curried ->
  Args args (Regs '[reg] r)
constructor1 f curried =
  argsN curried <&> \(reg, r) -> Regs (I (f reg) :* Nil) r 

constructor2 :: 
  forall r reg1' reg2' reg1 reg2 (args :: [Type]) curried.
  (MulticurryableF args (reg1', reg2', r) curried (IsFunction curried)) =>
  (reg1' -> reg1) ->
  (reg2' -> reg2) ->
  curried ->
  Args args (Regs '[reg1, reg2] r)
constructor2 f1 f2 curried =
  argsN curried <&> \(reg1, reg2, r) -> Regs (I (f1 reg1) :* I (f2 reg2) :* Nil) r 