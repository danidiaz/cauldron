{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

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
              do Recipe @args @bean recipe
              recipes
        }

data Recipe where
  Recipe ::
    (All Typeable args, Typeable bean) =>
    { 
      recipe :: Args args (Regs '[] bean)
    } ->
    Recipe

data RecipeReps = RecipeReps {
  argsReps :: [TypeRep],
  resultRep :: TypeRep
}

recipeReps :: Recipe -> RecipeReps
recipeReps Recipe {recipe} = recipeReps' recipe 

recipeReps' :: forall args result. (All Typeable args, Typeable result) =>
  Args args (Regs '[] result)
  -> RecipeReps
recipeReps' _ = 
  RecipeReps
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
  () <- first MissingRecipes checkMissing
  (beanGraph, plan) <- first RecipeCycle checkCycles
  let beans = build plan
  Right (BeanGraph {beanGraph}, beans)
  where
    checkMissing :: Either (Map TypeRep [TypeRep]) ()
    checkMissing =
      case Map.map
        do Prelude.filter (`Map.notMember` recipes)
        do (.argsReps) . recipeReps <$> recipes 
      of
      missing | Data.Foldable.any (not . Data.List.null) missing ->
        Left do missing
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
                  \beanRep (recipeReps -> RecipeReps {argsReps}) -> do
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
            let recipe = fromJust do Map.lookup rep recipes
                dyn = followRecipe dynMap recipe
             in Map.insert (dynTypeRep dyn) dyn dynMap
        Map.empty

data Mishap
  = 
    MissingRecipes (Map TypeRep [TypeRep])
  | RecipeCycle (NonEmpty TypeRep)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap TypeRep}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followRecipe :: Map TypeRep Dynamic -> Recipe -> Dynamic
followRecipe theDyns Recipe {recipe} = do
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor theDyns
      (_, bean) = runRegs do runArgs recipe args
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
