{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cauldron
  ( Cauldron,
    empty,
    put,
    boil,
    Mishap (..),
    BeanGraph (..),
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
import Data.Foldable qualified

newtype Cauldron = Cauldron { recipes :: Map TypeRep Recipe }

empty :: Cauldron
empty = Cauldron {recipes = Map.empty}

-- | Put a recipe (constructor) into the 'Cauldron'.
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
put recipe Cauldron {recipes} =
  let rep = typeRep (Proxy @bean)
   in Cauldron
        { recipes =
            Map.insert
              rep
              do makeRecipe @ingredients @bean recipe
              recipes
        }

data Recipe where
  Recipe ::
    (All Typeable ingredients, Typeable bean) =>
    { ingredientReps :: [TypeRep],
      beanRep :: TypeRep,
      uncurried :: NP I ingredients -> bean
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
      uncurried = multiuncurry @(->) @ingredients @bean @recipe recipe
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
        do (.ingredientReps) <$> recipes 
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
  = 
    MissingRecipes (Map TypeRep [TypeRep])
  | RecipeCycle (NonEmpty TypeRep)
  deriving stock (Show)

newtype BeanGraph = BeanGraph {beanGraph :: AdjacencyMap TypeRep}

-- | Build a bean out of already built beans.
-- This can only work without blowing up if there aren't dependecy cycles
-- and the order of construction respects the depedencies!
followRecipe :: Map TypeRep Dynamic -> Recipe -> Dynamic
followRecipe theDyns Recipe {uncurried} =
  let ingredientsExtractor = sequence_NP do cpure_NP (Proxy @Typeable) makeExtractor
      ingredients = runExtractor ingredientsExtractor theDyns
   in toDyn do uncurried ingredients

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
