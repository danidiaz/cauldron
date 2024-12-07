{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- {-# LANGUAGE TypeAbstractions #-}

module Cauldron.Constructor
  ( Constructor,
    arg,
    Beans,
    taste,
    fromDynList,
    toDynMap,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Control.Applicative
import Control.Monad.Fix
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Functor (($>), (<&>))
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..))
import Data.SOP (All, And, K (..))
import Data.SOP.NP
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Tree
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exts (IsList (..))
import Multicurryable
import Type.Reflection (SomeTypeRep (..), eqTypeRep)
import Type.Reflection qualified

data Constructor a = Constructor
  { runConstructor :: [Beans] -> Either TypeRep a
  }
  deriving (Functor)

arg :: forall a. (Typeable a) => Constructor a
arg =
  let tr = Type.Reflection.typeRep @a
      runConstructor bss =
        case asum do taste <$> bss of
          Just v -> Right v
          Nothing -> Left (SomeTypeRep tr)
   in Constructor {runConstructor}

taste :: forall a. (Typeable a) => Beans -> Maybe a
taste Beans {beanMap} =
  let tr = Type.Reflection.typeRep @a
   in case Map.lookup (SomeTypeRep tr) beanMap of
        Just (Dynamic tr' v) | Just HRefl <- tr `eqTypeRep` tr' -> Just v
        _ -> Nothing

instance Applicative Constructor where
  pure a = Constructor do pure do pure a
  Constructor f <*> Constructor a =
    Constructor do \beans -> f beans <*> a beans

newtype Beans = Beans {beanMap :: Map TypeRep Dynamic}

instance Semigroup Beans where
  Beans {beanMap = r1} <> Beans {beanMap = r2} = Beans do Map.unionWith (flip const) r1 r2

instance Monoid Beans where
  mempty = Beans mempty

fromDynList :: [Dynamic] -> Beans
fromDynList ds = Beans do Map.fromList do ds <&> \d -> (dynTypeRep d, d)

toDynMap :: Beans -> Map TypeRep Dynamic
toDynMap Beans {beanMap} = beanMap
