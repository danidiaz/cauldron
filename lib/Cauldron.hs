-- I'm using a lot of BlockArguments instead of $.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}

module Cauldron
  ( Cauldron,
    empty,
    put,
    cook,
    Mishap (..),
    taste,
    -- Re-exports
    TypeRep,
    AdjacencyMap
  )
where

-- import Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
-- import Algebra.Graph.AdjacencyIntMap qualified as IntGraph
-- import Algebra.Graph.AdjacencyIntMap.Algorithm qualified as IntGraph
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
import Data.List qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.SOP (All, K (..))
import Data.SOP.NP
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Tuple
import Data.Typeable
import Multicurryable
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty)
import Data.Dynamic

newtype Cauldron = Cauldron {recipes :: Map TypeRep Constructor}

empty :: Cauldron
empty = Cauldron do Map.empty

put ::
  forall (as :: [Type]) (b :: Type) curried.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b curried (IsFunction curried)
  ) =>
  curried ->
  Cauldron ->
  Cauldron
put curried Cauldron {recipes} =
  Cauldron do
    Map.insert
      do typeRep (Proxy @b)
      do makeConstructor @as @b @curried curried
      recipes

data Constructor where
  Constructor ::
    (All Typeable as, Typeable b) =>
    { argumentReps :: [TypeRep],
      resultRep :: TypeRep,
      uncurried :: NP I as -> b
    } ->
    Constructor

makeConstructor ::
  forall (as :: [Type]) (b :: Type) curried.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b curried (IsFunction curried)
  ) =>
  curried ->
  Constructor
makeConstructor curried =
  Constructor
    { argumentReps =
        collapse_NP do
          cpure_NP @_ @as
            do Proxy @Typeable
            typeRepHelper,
      resultRep = typeRep (Proxy @b),
      uncurried = multiuncurry @(->) @as @b @curried curried
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K do typeRep (Proxy @a)


cook :: Cauldron -> Either Mishap (AdjacencyMap TypeRep, Map TypeRep Dynamic)
cook Cauldron {recipes} =
  case findMissingDeps do (.argumentReps) <$> recipes of
    missing | Data.Foldable.any (not . Data.List.null) missing ->
      Left do MissingDeps missing
    _ ->
      let graph = 
            Graph.edges
            do flip Map.foldMapWithKey recipes
                  \resultRep Constructor {argumentReps} -> do
                    argumentRep <- argumentReps 
                    [(resultRep, argumentRep)]
       in case Graph.topSort graph of
        Left depCycle -> 
          Left do DepCycle depCycle
        Right buildPlan -> 
          let allConstructed = 
                Data.List.foldl'
                do \dynMap rep ->
                      let constructor = fromJust do Map.lookup rep recipes
                          dyn = construct dynMap constructor
                      in Map.insert (dynTypeRep dyn) dyn dynMap
                Map.empty
                buildPlan
           in Right (graph, allConstructed)
           

data Mishap = 
  MissingDeps (Map TypeRep [TypeRep])
  |DepCycle (NonEmpty TypeRep)
  deriving stock Show

findMissingDeps :: Map TypeRep [TypeRep] -> Map TypeRep [TypeRep]
findMissingDeps theMap =
  Map.map 
    do Prelude.filter (`Map.notMember` theMap)
    theMap

-- indexIsos :: Ord a => Set a -> (Int -> a, a -> Int)
-- indexIsos theSet = 
--   let numbered = zip [0 ..] do Set.toAscList theSet
--       indexMap = Map.fromAscList numbered
--       aMap = Map.fromAscList do Data.Tuple.swap <$> numbered
--    in (\i -> fromJust do Map.lookup i indexMap,
--        \a -> fromJust do Map.lookup a aMap
--       )
-- toIntMap ::
--   (Ord a) =>
--   Set a ->
--   (Map Int a, Map a Int)
-- toIntMap theSet =
--   let numbered = zip [0 ..] do Set.toAscList theSet
--    in ( Map.fromAscList numbered,
--         Map.fromAscList do Data.Tuple.swap <$> numbered
--       )

-- | Build a component out of already build compoenents.
-- This can only work and without blow up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!   
construct :: Map TypeRep Dynamic -> Constructor -> Dynamic
construct theDyns Constructor {uncurried} = 
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor 
      args = runExtractor argsExtractor theDyns
   in toDyn do uncurried args

newtype Extractor a = 
  Extractor { runExtractor :: Map TypeRep Dynamic -> a }
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. Typeable a => Extractor a
makeExtractor = 
  let rep = typeRep (Proxy @a)
      runExtractor dyns = 
        fromJust do fromDynamic @a do fromJust do Map.lookup rep dyns
   in Extractor {runExtractor}

taste :: forall a . Typeable a => Map TypeRep Dynamic -> a
taste = runExtractor makeExtractor
