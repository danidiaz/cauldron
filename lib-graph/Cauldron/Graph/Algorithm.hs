{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Cauldron.Graph.Algorithm (
    reverseTopSort
    ) where

import Cauldron.Graph
import Data.List.NonEmpty
import Data.Graph qualified
import Data.Foldable (for_)
import Data.Sequence qualified
import Data.Set qualified
import Data.Map.Strict qualified
import Data.Foldable qualified

reverseTopSort :: Ord a => AdjacencyMap a -> Either (NonEmpty a) [a] 
reverseTopSort g = do
    let theEdges = do
            (i,o) <- adjacencyList g 
            [(i,i,o)]
        sccs = Data.Graph.stronglyConnComp theEdges
    for_ sccs $ \case
        Data.Graph.AcyclicSCC _  -> pure ()
        Data.Graph.NECyclicSCC vs -> do
                let aCycle = findCycleInSCC g vs
                Left aCycle
    let (g',nodeFromVertex,_) = Data.Graph.graphFromEdges theEdges
    Right $ do
            ves <- Data.Graph.reverseTopSort g'
            let (v,_,_) = nodeFromVertex ves
            [v]

findCycleInSCC :: Ord a => AdjacencyMap a -> NonEmpty a -> NonEmpty a
findCycleInSCC g scc@(start :| _) = go start (Data.Set.singleton start) (Data.Sequence.singleton start)
  where
    sccSet = Data.Set.fromList . Data.Foldable.toList $ scc
    isInScc = (`Data.Set.member` sccSet)
    am = adjacencyMap $ Cauldron.Graph.induce isInScc g
    go current visited cycleAcc =
      case Data.Set.toList <$> Data.Map.Strict.lookup current am of
        Nothing -> error "findCycleInSCC: node not in adjacency map"
        Just [] -> error "findCycleInSCC: SCC node with no outgoing edge"
        Just (child:_) ->
          if child `Data.Set.member` visited
            then
              Data.List.NonEmpty.fromList $ Data.Foldable.toList $ Data.Sequence.takeWhileL (/= child) cycleAcc
            else
              go child (Data.Set.insert child visited) (cycleAcc Data.Sequence.|> child)
