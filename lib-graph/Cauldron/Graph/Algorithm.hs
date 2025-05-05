{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Cauldron.Graph.Algorithm (
    reverseTopSort
    ) where

import Cauldron.Graph
import Data.List.NonEmpty
import Data.Graph qualified
import Data.Foldable (for_)

reverseTopSort :: Ord a => AdjacencyMap a -> Either (NonEmpty a) [a] 
reverseTopSort am = do
    let theEdges = do
            (i,o) <- adjacencyList am 
            [(i,i,o)]
        sccs = Data.Graph.stronglyConnComp theEdges
    for_ sccs $ \case
        Data.Graph.AcyclicSCC _  -> pure ()
        Data.Graph.NECyclicSCC vs -> Left vs
    let (g,nodeFromVertex,_) = Data.Graph.graphFromEdges theEdges
    Right $ do
            ves <- Data.Graph.reverseTopSort g
            let (v,_,_) = nodeFromVertex ves
            [v]
