-- I'm using a lot of BlockArguments instead of $.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cauldron
  ( Cauldron,
    put,
  )
where

import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.SOP (All, K (..))
import Data.SOP.NP
import Data.Typeable
import Multicurryable
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
import Algebra.Graph.AdjacencyIntMap qualified as IntGraph
import Data.Traversable
import Data.List qualified
import Data.Foldable

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

cook :: Cauldron ->  Either MissingDeps (AdjacencyMap TypeRep)
cook Cauldron {recipes} =
  case findMissingDeps recipes of
    missing | not do Data.List.null do Data.Foldable.fold missing -> 
      Left do MissingDeps missing
    _ -> 
      undefined

--   let graph =
--         IntGraph.edges
--         do flip Map.foldMapWithKey (numbered recipes)
--               \resultRep (number, Constructor {argumentReps}) ->
--                 undefined
--    in undefined
  where
    numbered :: Ord a => Map a b -> Map a (Int, b)
    numbered theMap =
      let (_, result) = Map.mapAccum (\n b -> (succ n, (n,b))) 0 theMap
      in result

newtype MissingDeps = MissingDeps (Map TypeRep [TypeRep])

findMissingDeps :: Map TypeRep Constructor -> Map TypeRep [TypeRep]
findMissingDeps theMap =
  Map.map
  do \Constructor {argumentReps} -> filter (`Map.notMember` theMap) argumentReps
  theMap
