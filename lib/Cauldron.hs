-- I'm using BlockArguments a lot instead of $.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cauldron
  ( Cauldron,
    empty,
    put,
    cook,
    Mishap (..),
    Beans (..),
    taste,
    exportToDot,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable
import Data.Kind
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.SOP (All, K (..))
import Data.SOP.NP
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Typeable
import Multicurryable

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

cook :: Cauldron -> Either Mishap Beans
cook Cauldron {recipes} =
  case findMissingDeps do (.argumentReps) <$> recipes of
    missing | Data.Foldable.any (not . Data.List.null) missing ->
      Left do MissingDeps missing
    _ ->
      let depGraph =
            Graph.edges
              do
                flip
                  Map.foldMapWithKey
                  recipes
                  \resultRep Constructor {argumentReps} -> do
                    argumentRep <- argumentReps
                    [(resultRep, argumentRep)]
       in case Graph.topSort depGraph of
            Left depCycle ->
              Left do DepCycle depCycle
            Right buildPlan ->
              let beans =
                    Data.List.foldl'
                      do
                        \dynMap rep ->
                          let constructor = fromJust do Map.lookup rep recipes
                              dyn = construct dynMap constructor
                           in Map.insert (dynTypeRep dyn) dyn dynMap
                      Map.empty
                      buildPlan
               in Right Beans {depGraph, beans}

data Mishap
  = MissingDeps (Map TypeRep [TypeRep])
  | DepCycle (NonEmpty TypeRep)
  deriving stock (Show)

data Beans = Beans
  { depGraph :: AdjacencyMap TypeRep,
    beans :: Map TypeRep Dynamic
  }

findMissingDeps :: Map TypeRep [TypeRep] -> Map TypeRep [TypeRep]
findMissingDeps theMap =
  Map.map
    do Prelude.filter (`Map.notMember` theMap)
    theMap

-- | Build a component out of already build components.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
construct :: Map TypeRep Dynamic -> Constructor -> Dynamic
construct theDyns Constructor {uncurried} =
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor theDyns
   in toDyn do uncurried args

newtype Extractor a = Extractor {runExtractor :: Map TypeRep Dynamic -> a}
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let rep = typeRep (Proxy @a)
      runExtractor dyns =
        fromJust do fromDynamic @a do fromJust do Map.lookup rep dyns
   in Extractor {runExtractor}

taste :: forall a. (Typeable a) => Beans -> a
taste Beans {beans} = runExtractor makeExtractor beans

exportToDot :: FilePath -> Beans -> IO ()
exportToDot filepath Beans {depGraph} = do
  let dot =
        Dot.export
          do Dot.defaultStyle do \rep -> Data.Text.pack do tyConName do typeRepTyCon rep
          depGraph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)