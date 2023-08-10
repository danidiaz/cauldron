{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cauldron
  ( Cauldron,
    empty,
    put,
    cook,
    Mishap (..),
    BeanGraph (..),
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
  forall (ingredients :: [Type]) (bean :: Type) recipe.
  ( All Typeable ingredients,
    Typeable bean,
    MulticurryableF ingredients bean recipe (IsFunction recipe)
  ) =>
  -- | A curried function that takes the @ingredients@ and returns the @bean@
  recipe ->
  Cauldron ->
  Cauldron
put recipe Cauldron {recipes, recipesConflicts} =
  let rep = typeRep (Proxy @bean)
   in Cauldron
        { recipes =
            Map.insert
              rep
              do makeRecipe @ingredients @bean recipe
              recipes,
          recipesConflicts =
            case Map.lookup rep recipes of
              Nothing -> recipesConflicts
              _ -> Set.insert rep recipesConflicts
        }

data Recipe where
  Recipe ::
    (All Typeable ingredients, Typeable bean) =>
    { ingredientReps :: [TypeRep],
      beanRep :: TypeRep,
      unrecipe :: NP I ingredients -> bean
    } ->
    Recipe

makeRecipe ::
  forall (ingredients :: [Type]) (bean :: Type) recipe.
  ( All Typeable ingredients,
    Typeable bean,
    MulticurryableF ingredients bean recipe (IsFunction recipe)
  ) =>
  recipe ->
  Recipe
makeRecipe recipe =
  Recipe
    { ingredientReps =
        collapse_NP do
          cpure_NP @_ @ingredients
            do Proxy @Typeable
            typeRepHelper,
      beanRep = typeRep (Proxy @bean),
      unrecipe = multiuncurry @(->) @ingredients @bean @recipe recipe
    }
  where
    typeRepHelper :: forall a. (Typeable a) => K TypeRep a
    typeRepHelper = K do typeRep (Proxy @a)

type Plan = [TypeRep]

-- | Build a @bean@ from the recipes stored in the 'Cauldron'.
cook ::
  forall bean.
  (Typeable bean) =>
  Cauldron ->
  Either Mishap (BeanGraph, bean)
cook Cauldron {recipes, recipesConflicts} = do
  () <- first ConflictingRecipes checkConflicts
  () <- first MissingRecipes checkMissing
  (beanGraph, plan) <- first RecipeCycle checkCycles
  let beans = build plan
      bean :: bean = runExtractor makeExtractor beans
  Right (BeanGraph {beanGraph}, bean)
  where
    checkConflicts :: Either (Set TypeRep) ()
    checkConflicts =
      if not do Set.null recipesConflicts
        then Left do recipesConflicts
        else Right ()
    checkMissing :: Either (Set TypeRep) ()
    checkMissing =
      let allIngredients = Set.fromList do
            Recipe {ingredientReps} <- Map.elems recipes
            ingredientReps
          allRequired =
            Set.insert
              (typeRep (Proxy @bean))
              allIngredients
          missing = Set.filter (`Map.notMember` recipes) allRequired
       in if not (Set.null missing)
            then Left missing
            else Right ()
    checkCycles :: Either (Graph.Cycle TypeRep) (AdjacencyMap TypeRep, Plan)
    checkCycles =
      let beanGraph =
            Graph.edges
              do
                flip
                  Map.foldMapWithKey
                  recipes
                  \beanRep Recipe {ingredientReps} -> do
                    ingredientRep <- ingredientReps
                    [(beanRep, ingredientRep)]
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
  = ConflictingRecipes (Set TypeRep)
  | MissingRecipes (Set TypeRep)
  | RecipeCycle (NonEmpty TypeRep)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap TypeRep}

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

exportToDot :: FilePath -> BeanGraph -> IO ()
exportToDot filepath BeanGraph {beanGraph} = do
  let prettyRep rep =
        Data.Text.pack do tyConName do typeRepTyCon rep
      dot =
        Dot.export
          do Dot.defaultStyle prettyRep
          beanGraph
  Data.ByteString.writeFile filepath (Data.Text.Encoding.encodeUtf8 dot)
