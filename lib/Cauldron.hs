{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cauldron
  ( Cauldron,
    empty,
    put,
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
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Kind
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.SOP (All, K (..), And)
import Data.SOP.NP
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Typeable
import Multicurryable
import Data.Foldable qualified

newtype Cauldron = Cauldron { recipes :: Map TypeRep Recipe }

empty :: Cauldron
empty = Cauldron {recipes = Map.empty}

-- | Put a recipe (constructor) into the 'Cauldron'.
put ::
  forall (args :: [Type]) (bean :: Type).
  (All Typeable args, Typeable bean) =>
  -- | A curried function that takes the @args@ and returns the @bean@
  Args args (Regs '[] bean) ->
  Cauldron ->
  Cauldron
put recipe Cauldron {recipes} =
  let rep = typeRep (Proxy @bean)
   in Cauldron
        { recipes =
            Map.insert
              rep
              do Recipe { 
                  beanCon = Just do Constructor @args @bean recipe,
                  decosCons = []
              }
              recipes
        }

data Recipe where
  Recipe :: { 
    beanCon :: Maybe Constructor, 
    decosCons :: [Constructor]
  } -> Recipe

data Constructor where
  Constructor ::
    (All Typeable args, Typeable bean) =>
    { 
      constructor :: Args args (Regs '[] bean)
    } ->
    Constructor

data ConstructorReps = ConstructorReps {
  argsReps :: [TypeRep],
  resultRep :: TypeRep
}

-- https://discord.com/channels/280033776820813825/280036215477239809/1147832555828162594
-- https://github.com/ghc-proposals/ghc-proposals/pull/126#issuecomment-1363403330
constructorReps :: Constructor -> ConstructorReps
constructorReps Constructor {constructor = (_ :: Args args (Regs '[] result))} = 
  ConstructorReps
    { argsReps =
        collapse_NP do
          cpure_NP @_ @args
            do Proxy @Typeable
            typeRepHelper,
      resultRep = 
        typeRep (Proxy @result)
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K do typeRep (Proxy @a)

type Plan = [TypeRep]

-- | Try to build a @bean@ from the recipes stored in the 'Cauldron'.
boil ::
  Cauldron ->
  Either Mishap (BeanGraph, Map TypeRep Dynamic)
boil Cauldron {recipes} = do
  () <- first BeanlessDecorator checkBeanlessDecos
  () <- first MissingBeanDependencies checkMissingDeps
  (beanGraph, plan) <- first ConstructorCycle checkCycles
  let beans = build plan
  Right (BeanGraph {beanGraph}, beans)
  where
    checkBeanlessDecos :: Either (Set TypeRep) ()
    checkBeanlessDecos =
        case flip Map.foldMapWithKey
                recipes 
                do \beanRep -> \case 
                      Recipe { beanCon = Just _} -> Set.empty
                      _ -> Set.singleton beanRep
        of 
        missing | not do Data.List.null missing ->
          Left missing
        _ -> 
          Right ()
    checkMissingDeps :: Either (Map TypeRep [TypeRep]) ()
    checkMissingDeps =
      case Map.map
        do Prelude.filter (`Map.notMember` recipes)
        do (.argsReps) . constructorReps . fromJust . (.beanCon) <$> recipes 
      of
      missing | Data.Foldable.any (not . Data.List.null) missing ->
        Left missing
      _ ->
        Right ()
    checkCycles :: Either (Graph.Cycle TypeRep) (AdjacencyMap TypeRep, Plan)
    checkCycles =
      let beanGraph =
            Graph.edges
              do
                flip
                  Map.foldMapWithKey
                  recipes
                  \beanRep (constructorReps . fromJust . (.beanCon) -> ConstructorReps {argsReps}) -> do
                    argRep <- argsReps
                    [(beanRep, argRep)]
       in case Graph.topSort beanGraph of
            Left recipeCycle ->
              Left recipeCycle
            Right plan -> Right (beanGraph, plan)
    build :: Plan -> Map TypeRep Dynamic
    build =
      Data.List.foldl'
        do
          \dynMap rep ->
            let constructor = fromJust do (.beanCon) do fromJust do Map.lookup rep recipes
                dyn = followConstructor dynMap constructor
             in Map.insert (dynTypeRep dyn) dyn dynMap
        Map.empty

data Mishap
  = BeanlessDecorator (Set TypeRep) 
  | MissingBeanDependencies (Map TypeRep [TypeRep])
  | ConstructorCycle (NonEmpty TypeRep)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap TypeRep}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followConstructor :: Map TypeRep Dynamic -> Constructor -> Dynamic
followConstructor theDyns Constructor {constructor} = do
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor theDyns
      (_, bean) = runRegs do runArgs constructor args
  toDyn bean

newtype Extractor a = Extractor {runExtractor :: Map TypeRep Dynamic -> a}
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. Typeable a => Extractor a
makeExtractor =
  let runExtractor dyns =
        fromJust do taste (Proxy @a) dyns
   in Extractor {runExtractor}

exportToDot :: FilePath -> BeanGraph -> IO ()
exportToDot filepath BeanGraph {beanGraph} = do
  let prettyRep rep =
        Data.Text.pack do tyConName do typeRepTyCon rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          beanGraph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)

taste :: forall a. Typeable a => Proxy a -> Map TypeRep Dynamic -> Maybe a
taste _ dyns = do
  let rep = typeRep (Proxy @a)
  dyn <- Map.lookup rep dyns
  fromDynamic @a dyn

newtype Args args r = 
  Args { runArgs :: NP I args -> r }
  deriving newtype (Functor, Applicative, Monad)

args0 :: r -> Args '[] r
args0 r = Args do \_ -> r

argsN :: forall (args :: [Type]) r curried.
    MulticurryableF args r curried (IsFunction curried) =>
    curried -> Args args r
argsN = Args . multiuncurry

newtype Regs (regs :: [Type]) r = 
  Regs { runRegs :: (NP I regs, r) }
  deriving newtype Functor

regs0 :: r -> Regs '[] r
regs0 r = Regs (Nil, r)
