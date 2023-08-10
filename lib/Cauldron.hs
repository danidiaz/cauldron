-- I'm using BlockArguments a lot instead of $.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Bifunctor (first)
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Typeable
import Multicurryable

data Cauldron = Cauldron
  { recipes :: Map TypeRep Recipe,
    recipesConflicts :: Set TypeRep
  }

empty :: Cauldron
empty = Cauldron {recipes = Map.empty, recipesConflicts = Set.empty}

-- | Might throw a 'ConflictingBeanRecipes' exception.
put ::
  forall (as :: [Type]) (b :: Type) recipe.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b recipe (IsFunction recipe)
  ) =>
  recipe ->
  Cauldron ->
  Cauldron
put recipe Cauldron {recipes, recipesConflicts} =
  let rep = typeRep (Proxy @b)
   in Cauldron
        { recipes =
            Map.insert
              rep
              do makeRecipe @as @b @recipe recipe
              recipes,
          recipesConflicts =
            case Map.lookup rep recipes of
              Nothing -> recipesConflicts
              _ -> Set.insert rep recipesConflicts
        }

data Recipe where
  Recipe ::
    (All Typeable as, Typeable b) =>
    { ingredientReps :: [TypeRep],
      beanRep :: TypeRep,
      unrecipe :: NP I as -> b
    } ->
    Recipe

makeRecipe ::
  forall (as :: [Type]) (b :: Type) recipe.
  ( All Typeable as,
    Typeable b,
    MulticurryableF as b recipe (IsFunction recipe)
  ) =>
  recipe ->
  Recipe
makeRecipe recipe =
  Recipe
    { ingredientReps =
        collapse_NP do
          cpure_NP @_ @as
            do Proxy @Typeable
            typeRepHelper,
      beanRep = typeRep (Proxy @b),
      unrecipe = multiuncurry @(->) @as @b @recipe recipe
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K do typeRep (Proxy @a)

type Plan = [TypeRep]

cook :: Cauldron -> Either Mishap Beans
cook Cauldron {recipes, recipesConflicts} = do
  () <- first ConflictingRecipes checkConflicts
  () <- first MissingRecipes checkMissing
  (recipeGraph, plan) <- first RecipeCycle checkCycles
  let beans = build plan
  Right Beans {recipeGraph, beans}
  where
    checkConflicts :: Either (Set TypeRep) ()
    checkConflicts =
      if not do Set.null recipesConflicts
        then Left do recipesConflicts
        else Right ()
    checkMissing :: Either (Map TypeRep [TypeRep]) ()
    checkMissing =
      case Map.map
        do Prelude.filter (`Map.notMember` recipes)
        do (.ingredientReps) <$> recipes of
        missing | Data.Foldable.any (not . Data.List.null) missing ->
          Left do missing
        _ ->
          Right ()
    checkCycles :: Either (Graph.Cycle TypeRep) (AdjacencyMap TypeRep, Plan)
    checkCycles =
      let recipeGraph =
            Graph.edges
              do
                flip
                  Map.foldMapWithKey
                  recipes
                  \beanRep Recipe {ingredientReps} -> do
                    ingredientRep <- ingredientReps
                    [(beanRep, ingredientRep)]
       in case Graph.topSort recipeGraph of
            Left recipeCycle ->
              Left recipeCycle
            Right plan -> Right (recipeGraph, plan)
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
  = ConflictingRecipes (Set TypeRep)
  | MissingRecipes (Map TypeRep [TypeRep])
  | RecipeCycle (NonEmpty TypeRep)
  deriving stock (Show)

data Beans = Beans
  { recipeGraph :: AdjacencyMap TypeRep,
    beans :: Map TypeRep Dynamic
  }

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followRecipe :: Map TypeRep Dynamic -> Recipe -> Dynamic
followRecipe theDyns Recipe {unrecipe} =
  let argsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      args = runExtractor argsExtractor theDyns
   in toDyn do unrecipe args

newtype Extractor a = Extractor {runExtractor :: Map TypeRep Dynamic -> a}
  deriving newtype (Functor, Applicative)

makeExtractor :: forall a. (Typeable a) => Extractor a
makeExtractor =
  let rep = typeRep (Proxy @a)
      runExtractor dyns =
        fromJust do fromDynamic @a do fromJust do Map.lookup rep dyns
   in Extractor {runExtractor}

-- | Might throw an error if the bean is not present!
taste :: forall a. (Typeable a) => Beans -> a
taste Beans {beans} = runExtractor makeExtractor beans

exportToDot :: FilePath -> Beans -> IO ()
exportToDot filepath Beans {recipeGraph} = do
  let prettyRep rep =
        Data.Text.pack do tyConName do typeRepTyCon rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          recipeGraph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)