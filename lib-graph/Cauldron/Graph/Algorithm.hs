{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Cauldron.Graph.Algorithm (
    reverseTopSort
    ) where

import Cauldron.Graph
import Data.List.NonEmpty
import Data.Graph qualified
import Data.Foldable (for_)
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.List qualified
import Data.Set (Set)
import Data.Set qualified
import Data.Map (Map)
import Data.Map.Strict qualified

reverseTopSort :: Ord a => AdjacencyMap a -> Either (NonEmpty a) [a] 
reverseTopSort am = do
    let theEdges = do
            (i,o) <- adjacencyList am 
            [(i,i,o)]
        sccs = Data.Graph.stronglyConnComp theEdges
    for_ sccs $ \case
        Data.Graph.AcyclicSCC _  -> pure ()
        Data.Graph.NECyclicSCC vs -> 
            do 
                let aCycle = findCycleInSCC (adjacencyMap am) vs
                Left aCycle
    let (g,nodeFromVertex,_) = Data.Graph.graphFromEdges theEdges
    Right $ do
            ves <- Data.Graph.reverseTopSort g
            let (v,_,_) = nodeFromVertex ves
            [v]

findCycleInSCC :: Ord a => AdjacencyMap a -> NonEmpty a -> NonEmpty a
findCycleInSCC g scc@(start :| _) = go start [start] (Data.Set.singleton start)
  where
    sccSet = Data.Set.fromList . toList $ scc
    isInScc = (`Data.Set.member` sccSet)
    am = adjacencyMap $ Cauldron.Graph.induce isInScc g
    go current acc visited =
      case Data.Set.toList <$> Data.Map.Strict.lookup current am of
        Nothing -> error "findCycleInSCC: node not in adjacency map"
        Just [] -> error "findCycleInSCC: SCC node with no outgoing edge"
        Just (child:_) ->
          if child `Data.Set.member` visited
            then
              -- Prune acc up to the first occurrence of child, then return the cycle
              let (_before, aCycle) = Data.List.break (== child) (Data.List.reverse acc)
              in Data.List.NonEmpty.fromList  aCycle
            else
              go child (child:acc) (Data.Set.insert child visited)
