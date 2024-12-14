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
    constructor,
    effectfulConstructor,
    constructorWithRegs,
    effectfulConstructorWithRegs,
    runConstructor,
    hoistConstructor,
    required,
    provided,
    Args,
    arg,
    reg,
    Regs,
    Beans,
    taste,
    fromDynList,
    toDynMap,
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

newtype Constructor m a = Constructor (Args (m (Regs a)))
  deriving (Functor)

constructor :: (Applicative m) => Args bean -> Constructor m bean
constructor x = Constructor $ fmap (pure . pure) x

effectfulConstructor :: (Functor m) => Args (m bean) -> Constructor m bean
effectfulConstructor x = Constructor $ fmap (fmap pure) x

constructorWithRegs :: (Applicative m) => Args (Regs bean) -> Constructor m bean
constructorWithRegs x = Constructor $ fmap pure x

effectfulConstructorWithRegs :: (Functor m) => Args (m (Regs bean)) -> Constructor m bean
effectfulConstructorWithRegs x = Constructor x

runConstructor :: (Monad m) => [Beans] -> Constructor m bean -> Either TypeRep (m (Beans, bean))
runConstructor beans (Constructor Args {regReps, runArgs}) =
  case runArgs beans of
    Left tr -> Left tr
    Right action -> Right do
      Regs regBeans bean <- action
      let onlyStaticlyKnown =
            Map.restrictKeys regBeans regReps
              `Map.union` Map.fromSet someMonoidTypeRepMempty regReps -- remember that union is left-biased!!!!
      pure (Beans $ Map.mapKeys someMonoidTypeRepToSomeTypeRep onlyStaticlyKnown, bean)

-- | Change the monad in which the 'Constructor'\'s effects take place.
hoistConstructor :: (forall x. m x -> n x) -> Constructor m bean -> Constructor n bean
hoistConstructor f (Constructor theArgs) = Constructor do fmap f theArgs

required :: Constructor m a -> Set SomeTypeRep
required (Constructor (Args {argReps})) = argReps

provided :: Constructor m a -> Set SomeMonoidTypeRep
provided (Constructor (Args {regReps})) = regReps

data Args a = Args
  { argReps :: Set SomeTypeRep,
    regReps :: Set SomeMonoidTypeRep,
    runArgs :: [Beans] -> Either TypeRep a
  }
  deriving (Functor)

arg :: forall a. (Typeable a) => Args a
arg =
  let tr = typeRep (Proxy @a)
   in Args
        { argReps = Set.singleton tr,
          regReps = Set.empty,
          runArgs = \bss ->
            case asum do taste <$> bss of
              Just v -> Right v
              Nothing -> Left tr
        }

reg :: forall a. (Typeable a, Monoid a) => Args (a -> Regs ())
reg =
  let tr = SomeMonoidTypeRep (Type.Reflection.typeRep @a)
   in Args
        { argReps = Set.empty,
          regReps = Set.singleton tr,
          runArgs = pure $ pure \a ->
            Regs (Map.singleton tr (toDyn a)) ()
        }

taste :: forall a. (Typeable a) => Beans -> Maybe a
taste Beans {beanMap} =
  let tr = Type.Reflection.typeRep @a
   in case Map.lookup (SomeTypeRep tr) beanMap of
        Just (Dynamic tr' v) | Just HRefl <- tr `eqTypeRep` tr' -> Just v
        _ -> Nothing

instance Applicative Args where
  pure a =
    Args
      { argReps = Set.empty,
        regReps = Set.empty,
        runArgs = pure do pure a
      }
  Args
    { argReps = argReps1,
      regReps = regReps1,
      runArgs = f
    }
    <*> Args
      { argReps = argReps2,
        regReps = regReps2,
        runArgs = a
      } =
      Args
        { argReps = argReps1 `Set.union` argReps2,
          regReps = regReps1 `Set.union` regReps2,
          runArgs = \beans -> f beans <*> a beans
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

someMonoidTypeRepMempty :: SomeMonoidTypeRep -> Dynamic
someMonoidTypeRepMempty (SomeMonoidTypeRep @t _) = toDyn (mempty @t)

someMonoidTypeRepToSomeTypeRep :: SomeMonoidTypeRep -> SomeTypeRep
someMonoidTypeRepToSomeTypeRep (SomeMonoidTypeRep tr) = SomeTypeRep tr

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
