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

-- {-# LANGUAGE TypeAbstractions #-}

module Cauldron.Constructor
  ( Constructor,
    argReps,
    regReps,
    runConstructor,
    arg,
    reg,
    Beans,
    taste,
    fromDynList,
    toDynMap,
    Regs,
    runRegs,
    SomeMonoidTypeRep (..),
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
import GHC.IsList
import Multicurryable
import Type.Reflection (SomeTypeRep (..), eqTypeRep)
import Type.Reflection qualified

data Constructor a = Constructor
  { argReps :: Set TypeRep,
    regReps :: Set SomeMonoidTypeRep,
    runConstructor :: [Beans] -> Either TypeRep a
  }
  deriving (Functor)

arg :: forall a. (Typeable a) => Constructor a
arg =
  let tr = typeRep (Proxy @a)
   in Constructor
        { argReps = Set.singleton tr,
          regReps = Set.empty,
          runConstructor = \bss ->
            case asum do taste <$> bss of
              Just v -> Right v
              Nothing -> Left tr
        }

reg :: forall a. (Typeable a, Monoid a) => Constructor (a -> Regs ())
reg =
  let tr = SomeMonoidTypeRep (Type.Reflection.typeRep @a)
   in Constructor
        { argReps = Set.empty,
          regReps = Set.singleton tr,
          runConstructor = pure $ pure \a ->
            Regs (Map.singleton tr (toDyn a)) ()
        }

taste :: forall a. (Typeable a) => Beans -> Maybe a
taste Beans {beanMap} =
  let tr = Type.Reflection.typeRep @a
   in case Map.lookup (SomeTypeRep tr) beanMap of
        Just (Dynamic tr' v) | Just HRefl <- tr `eqTypeRep` tr' -> Just v
        _ -> Nothing

instance Applicative Constructor where
  pure a =
    Constructor
      { argReps = Set.empty,
        regReps = Set.empty,
        runConstructor = pure do pure a
      }
  Constructor
    { argReps = argReps1,
      regReps = regReps1,
      runConstructor = f
    }
    <*> Constructor
      { argReps = argReps2,
        regReps = regReps2,
        runConstructor = a
      } =
      Constructor
        { argReps = argReps1 `Set.union` argReps2,
          regReps = regReps1 `Set.union` regReps2,
          runConstructor = \beans -> f beans <*> a beans
        }

newtype Beans = Beans {beanMap :: Map TypeRep Dynamic}

instance Semigroup Beans where
  Beans {beanMap = r1} <> Beans {beanMap = r2} = Beans do Map.unionWith (flip const) r1 r2

instance Monoid Beans where
  mempty = Beans mempty

instance IsList Beans where
  type Item Beans = Dynamic
  toList (Beans {beanMap}) = Map.elems beanMap
  fromList = fromDynList

fromDynList :: [Dynamic] -> Beans
fromDynList ds = Beans do Map.fromList do ds <&> \d -> (dynTypeRep d, d)

toDynMap :: Beans -> Map TypeRep Dynamic
toDynMap Beans {beanMap} = beanMap

--
data SomeMonoidTypeRep where
  SomeMonoidTypeRep ::
    forall a.
    (Typeable a, Monoid a) =>
    Type.Reflection.TypeRep a ->
    SomeMonoidTypeRep

instance Show SomeMonoidTypeRep where
  show (SomeMonoidTypeRep tr) = show tr

instance Eq SomeMonoidTypeRep where
  (SomeMonoidTypeRep tr1) == (SomeMonoidTypeRep tr2) =
    (SomeTypeRep tr1) == (SomeTypeRep tr2)

instance Ord SomeMonoidTypeRep where
  (SomeMonoidTypeRep tr1) `compare` (SomeMonoidTypeRep tr2) =
    (SomeTypeRep tr1) `compare` (SomeTypeRep tr2)

-- | Unrestricted building SHOULD NOT be public!
data Regs a = Regs (Map SomeMonoidTypeRep Dynamic) a
  deriving (Functor)

instance Applicative Regs where
  pure a = Regs Map.empty a
  Regs w1 f <*> Regs w2 a2 =
    Regs (Map.unionWithKey combineMonoidRegs w1 w2) (f a2)

instance Monad Regs where
  (Regs w1 a) >>= k =
    let Regs w2 r = k a
     in Regs (Map.unionWithKey combineMonoidRegs w1 w2) r

combineMonoidRegs :: SomeMonoidTypeRep -> Dynamic -> Dynamic -> Dynamic
combineMonoidRegs mtr d1 d2 = case (mtr, d1, d2) of
  (SomeMonoidTypeRep tr, Dynamic tr1 v1, Dynamic tr2 v2)
    | Just HRefl <- tr `eqTypeRep` tr1,
      Just HRefl <- tr `eqTypeRep` tr2 ->
        toDyn (v1 <> v2)
  _ -> error "impossible"

runRegs :: Regs a -> (Map SomeMonoidTypeRep Dynamic, a)
runRegs (Regs w a) = (w, a)
