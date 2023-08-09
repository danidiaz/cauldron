-- I'm using a lot of BlockArguments instead of $.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

module Cauldron
  ( Cauldron,
    put,
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

newtype Cauldron = Cauldron {recipes :: Map TypeRep Constructor}

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
      do constructor @as @b @curried curried
      recipes

data Constructor where
  Constructor ::
    (All Typeable as, Typeable b) =>
    { argumentReps :: [TypeRep],
      resultRep :: TypeRep,
      uncurried :: NP I as -> b
    } ->
    Constructor

constructor ::
  forall (as :: [Type]) (b :: Type) curried.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b curried (IsFunction curried)
  ) =>
  curried ->
  Constructor
constructor curried =
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

cook :: Cauldron -> Either Mishap (AdjacencyMap TypeRep)
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
        Right buildPlan -> undefined

data Mishap = 
  MissingDeps (Map TypeRep [TypeRep])
  |DepCycle (NonEmpty TypeRep)

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
