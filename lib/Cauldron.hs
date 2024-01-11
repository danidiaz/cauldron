{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | A library for performing dependency injection.
module Cauldron
  ( Cauldron,
    insert,
    adjust,
    delete,
    cook,
    cookNonEmpty,
    cookTree,
    Bean (..),
    makeBean,
    setConstructor,
    setDecos,
    overDecos,
    Decos,
    addInner,
    addOuter,
    fromConstructors,
    Constructor,
    pack,
    packPure,
    Packer,
    simple,
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
    -- | The With monad for handling resources.
    With,
    with,
    runWith,
    -- | Re-exports
    mempty
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
-- import Control.Monad.Fix
import Data.Functor.Compose
import Control.Applicative
import Data.Tree
import Data.Functor (($>), (<&>))
import Control.Monad.IO.Class
import Control.Exception (throwIO, catch)
import Control.Exception.Base (BlockedIndefinitelyOnMVar (..),  FixIOException(..))
import Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Control.Monad.Trans.Class
import Control.Monad.Fix
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

addInner :: Constructor m bean -> Decos m bean -> Decos m bean
addInner con (Decos {decoCons}) = Decos do con Seq.<| decoCons

addOuter :: Constructor m bean -> Decos m bean -> Decos m bean
addOuter con (Decos {decoCons}) = Decos do decoCons Seq.|> con

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
    { 
      beanRep :: TypeRep,
      argReps :: Set TypeRep,
      regReps :: Map TypeRep Dynamic
    } ->
    ConstructorReps

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
-- | This function DOESN'T return the bean rep itself in the argreps.
constructorReps :: (Typeable bean) => Constructor m bean -> ConstructorReps
constructorReps Constructor {constructor_ = (_ :: Args args (m (Regs accums bean)))} =
  ConstructorReps
    { 
      beanRep,
      argReps = 
        Set.delete beanRep do 
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

type Plan = [PlanItem]

data PlanItem
  = BareBean TypeRep
  | BeanDecorator TypeRep Int
  | BuiltBean TypeRep
  deriving stock (Show, Eq, Ord)

newtype BoiledBeans where
  BoiledBeans :: {beans :: Map TypeRep Dynamic} -> BoiledBeans

-- | Build the beans using the constructors stored in the 'Cauldron'.
--
-- Continuation-based monads like ContT or Codensity can be used here, but
-- _only_ to wrap @withFoo@-like helpers. Weird hijinks like running the
-- continuation _twice_ will dealock or throw an exeption.
cook :: forall m.
  MonadFix m =>
  Cauldron m ->
  Either BadBeans (DependencyGraph, m BoiledBeans)
cook cauldron = do
  let result = cookTree (Node cauldron [])
  result <&> \(tg, m) -> (rootLabel tg, rootLabel <$> m)

cookNonEmpty :: forall m.
  MonadFix m =>
  NonEmpty (Cauldron m) ->
  Either BadBeans (NonEmpty DependencyGraph, m (NonEmpty BoiledBeans))
cookNonEmpty nonemptyCauldronList = do
  let result = cookTree (nonEmptyToTree nonemptyCauldronList)
  result <&> \(ng, m) -> (unsafeTreeToNonEmpty ng, unsafeTreeToNonEmpty <$> m)

cookTree :: forall m .
  (MonadFix m) =>
  Tree (Cauldron m) ->
  Either BadBeans (Tree DependencyGraph, m (Tree (BoiledBeans)))
cookTree (fmap (.recipes) -> treecipes) = do
  accumMap <- first DoubleDutyBeans do checkNoDoubleDutyBeans treecipes
  () <- first MissingDependencies do checkMissingDeps (Map.keysSet accumMap) treecipes
  treeplan <- first DependencyCycle do buildPlans treecipes
  Right
    ( treeplan <&> \(graph,_) -> DependencyGraph {graph},
      do
        treebeans <- followPlan accumMap (snd <$> treeplan)
        pure do BoiledBeans <$> treebeans 
    )

checkNoDoubleDutyBeans ::
  Tree (Map TypeRep (SomeBean m)) ->
  Either (Set TypeRep) (Map TypeRep Dynamic)
checkNoDoubleDutyBeans treecipes = do
  let (accumMap, beanSet) = cauldronTreeRegs treecipes
  let common = Set.intersection (Map.keysSet accumMap) beanSet
  if not (Set.null common)
    then Left common
    else Right accumMap

type TreeKey = [Int]

decorate :: 
  (TreeKey, Map TypeRep TreeKey, Tree (Map TypeRep (SomeBean m))) -> 
  Tree (Map TypeRep TreeKey, Map TypeRep (SomeBean m))
decorate = unfoldTree 
  do \(key, acc, Node current rest) -> 
        let -- current level has priority
            newAcc = (current $> key) `Map.union` acc
            newSeeds = do
              (i, z) <- zip [0..] rest 
              let newKey = key ++ [i]
              [(newKey, newAcc , z)]
         in ((newAcc, current), newSeeds)

cauldronTreeRegs :: Tree (Map TypeRep (SomeBean m)) -> (Map TypeRep Dynamic, Set TypeRep)
cauldronTreeRegs = foldMap cauldronRegs 

cauldronRegs :: Map TypeRep (SomeBean m) -> (Map TypeRep Dynamic, Set TypeRep)
cauldronRegs = 
  Map.foldMapWithKey \rep recipe -> (recipeRegs recipe, Set.singleton rep)

-- | Returns the accumulators, not the main bean
recipeRegs :: SomeBean m -> Map TypeRep Dynamic
recipeRegs (SomeBean Bean {constructor, decos = Decos {decoCons}}) = do
    let extractRegReps = (.regReps) . constructorReps
    extractRegReps constructor 
      <> foldMap extractRegReps decoCons

checkMissingDeps ::
  -- | accums 
  Set TypeRep ->
  Tree (Map TypeRep (SomeBean m)) ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDeps accums treecipes = do
  let decoratedTreecipes = decorate ([], Map.empty, treecipes)
      missing = (\(available,requested) -> checkMissingDepsCauldron accums (Map.keysSet available) requested) <$> decoratedTreecipes
  sequence_ missing

checkMissingDepsCauldron ::
  -- | accums 
  Set TypeRep ->
  -- | available at this level
  Set TypeRep ->
  Map TypeRep (SomeBean m) ->
  Either (Map TypeRep (Set TypeRep)) ()
checkMissingDepsCauldron accums available recipes = do
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

buildPlans :: Tree (Map TypeRep (SomeBean m)) -> Either (NonEmpty PlanItem) (Tree (AdjacencyMap PlanItem, (Plan, Map TypeRep (SomeBean m))))
buildPlans = traverse \recipes -> do
  let graph = buildDepGraphCauldron recipes
  case Graph.topSort graph of
    Left recipeCycle ->
      Left recipeCycle
    Right (reverse -> plan) -> Right (graph, (plan, recipes))

buildDepGraphCauldron :: Map TypeRep (SomeBean m) -> AdjacencyMap PlanItem
buildDepGraphCauldron recipes = Graph.edges 
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
                (decoIndex, decoCon) <- zip [0 :: Int ..] (Data.Foldable.toList decoCons)
                [(BeanDecorator beanRep decoIndex, decoCon)]
              beanDeps = constructorEdges bareBean constructor
              decoDeps = concatMap (uncurry constructorEdges) decos
              full = bareBean Data.List.NonEmpty.:| (fst <$> decos) ++ [builtBean]
              innerDeps = zip (Data.List.NonEmpty.tail full) (Data.List.NonEmpty.toList full)
          beanDeps ++ decoDeps ++ innerDeps

constructorEdges ::forall bean m.
  (Typeable bean) => 
  PlanItem ->
  Constructor m bean ->
  [(PlanItem, PlanItem)]
constructorEdges item (constructorReps -> ConstructorReps {argReps, regReps}) =
  -- consumers depend on their args
  (do
      argRep <- Set.toList argReps
      let argItem = BuiltBean argRep
      [(item, argItem)]
  ) ++
    -- regs depend on their producers
    ( do
        (regRep, _) <- Map.toList regReps
        let repItem = BuiltBean regRep
        [(repItem, item)]
    )

followPlan :: MonadFix m =>
  Map TypeRep Dynamic ->
  (Tree (Plan, Map TypeRep (SomeBean m))) ->
  m (Tree (Map TypeRep Dynamic))
followPlan initial treecipes =
  unfoldTreeM 
  (\(initial', Node (plan, cauldron) rest) -> do
      newInitial' <- followPlanCauldron cauldron initial' plan
      pure (newInitial', (,) newInitial' <$> rest))
  (initial, treecipes)

followPlanCauldron :: MonadFix m =>
  Map TypeRep (SomeBean m) ->
  Map TypeRep Dynamic ->
  Plan ->
  m (Map TypeRep Dynamic)
followPlanCauldron recipes initial plan = 
  mfix do \final -> Data.Foldable.foldlM
                  do followPlanStep recipes final
                  initial
                  plan

followPlanStep :: Monad m =>
 Map TypeRep (SomeBean m) ->
 Map TypeRep Dynamic -> 
 Map TypeRep Dynamic -> 
 PlanItem -> 
 m (Map TypeRep Dynamic)
followPlanStep recipes final super = \case
  BareBean rep -> case fromJust do Map.lookup rep recipes of
    SomeBean (Bean {constructor}) -> do
      let ConstructorReps {beanRep} = constructorReps constructor
      -- We delete the beanRep before running the constructor, 
      -- because if we have a self-dependency, we don't want to use the bean
      -- from a previous context (if it exists) we want the bean from final.
      (super', bean) <- followConstructor constructor final (Map.delete beanRep super)
      pure do Map.insert beanRep (toDyn bean) super'
  BuiltBean _ -> pure super
  BeanDecorator rep index -> case fromJust do Map.lookup rep recipes of
    SomeBean (Bean {decos = Decos {decoCons}}) -> do
      let decoCon = fromJust do Seq.lookup index decoCons
      let ConstructorReps {beanRep} = constructorReps decoCon
      -- Unlike before, we don't delete the beanRep before running the constructor.
      (super', bean) <- followConstructor decoCon final super
      pure do Map.insert  beanRep (toDyn bean) super'

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor :: Monad m =>
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
              BareBean rep -> p rep <> Data.Text.pack "#bare"
              BeanDecorator rep index -> p rep <> Data.Text.pack ("#deco#" ++ show index)
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

type Packer m regs bean r = r -> m (Regs regs bean)

simple :: Applicative m => Packer m '[] bean bean 
simple bean = pure do regs0 bean

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

nonEmptyToTree :: NonEmpty a -> Tree a
nonEmptyToTree = \case
  a Data.List.NonEmpty.:| [] -> Node a []
  a Data.List.NonEmpty.:| (b : rest) -> Node a [nonEmptyToTree (b Data.List.NonEmpty.:| rest)]

unsafeTreeToNonEmpty :: Tree a -> NonEmpty a
unsafeTreeToNonEmpty = \case
  Node a [] -> a Data.List.NonEmpty.:| []
  Node a [b] -> Data.List.NonEmpty.cons a (unsafeTreeToNonEmpty b)
  _ -> error "tree not list-shaped"

-- dodgyFixIO :: MonadIO m => (a -> m a) -> m a
-- dodgyFixIO k = do
--     m <- liftIO $ newEmptyMVar
--     ans <- liftIO $ unsafeDupableInterleaveIO
--              (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
--                                     throwIO FixIOException)
--     result <- k ans
--     liftIO $ putMVar m result
--     return result


newtype With (m :: Type -> Type) a = With (forall b. (a -> m b) -> m b)

-- | Build a 'With' from a @withFoo@-style resource-handling function that 
-- accepts a continuation, like 'System.IO.withFile'.
-- 
-- Passing functions that do weird things like running their continuation
-- _twice_ might cause weird / erroneous behavior. But why would you want to do
-- that?
with :: (forall b. (a -> m b) -> m b) -> With m a
with = With

-- | This instance might be a little dodgy (cont-like monads don't really have
-- 'MonadFix' instances). Follow the recommendations given for 'with'.
instance (MonadFix m) => MonadFix (With m) where
    -- I don't pretendt to fully understand this.
    -- https://stackoverflow.com/a/25839026/1364288
    mfix f = With (\k -> mfixing (\a -> (do runWith (f a) k) <&> do (,a)))
      where 
        mfixing :: MonadFix z => (t -> z (b, t)) -> z b
        mfixing z = fst <$> mfix do \ ~(_,a) -> z a
    {-# INLINE mfix #-}

runWith :: With m a -> forall b. (a -> m b) -> m b
runWith (With r) = r 

instance Functor (With m) where
  fmap f (With m) = With (\k -> m (\x -> k (f x)))
  {-# INLINE fmap #-}

instance Applicative (With m) where
  pure x = With (\k -> k x)
  {-# INLINE pure #-}
  With f <*> With g = With (\bfr -> f (\ab -> g (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}

instance Monad (With m) where
  return = pure
  {-# INLINE return #-}
  m >>= k = With (\c -> runWith m (\a -> runWith (k a) c))
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (With m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans With where
  lift m = With (m >>=)
  {-# INLINE lift #-}
