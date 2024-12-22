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

module Cauldron.Beans
  ( Beans,
    empty,
    insert,
    delete,
    restrictKeys,
    keysSet,
    singleton,
    taste,
    fromDynList,
    toDynMap,
    unionBeansMonoidally,
    SomeMonoidTypeRep (..),
  )
where

import Data.Dynamic
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import GHC.IsList
import Type.Reflection (SomeTypeRep (..), eqTypeRep)
import Type.Reflection qualified

empty :: Beans
empty = Beans Map.empty

insert :: forall bean. (Typeable bean) => bean -> Beans -> Beans
insert bean Beans {beanMap} =
  Beans {beanMap = Map.insert (typeRep (Proxy @bean)) (toDyn bean) beanMap}

delete :: TypeRep -> Beans -> Beans
delete tr Beans {beanMap} =
  Beans {beanMap = Map.delete tr beanMap}

restrictKeys :: Beans -> Set TypeRep -> Beans
restrictKeys Beans {beanMap} trs = Beans {beanMap = Map.restrictKeys beanMap trs}

singleton :: forall a. (Typeable a) => a -> Beans
singleton bean = Beans do Map.singleton (typeRep (Proxy @a)) (toDyn bean)

taste :: forall a. (Typeable a) => Beans -> Maybe a
taste Beans {beanMap} =
  let tr = Type.Reflection.typeRep @a
   in case Map.lookup (SomeTypeRep tr) beanMap of
        Just (Dynamic tr' v) | Just HRefl <- tr `eqTypeRep` tr' -> Just v
        _ -> Nothing

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

unionBeansMonoidally :: Set SomeMonoidTypeRep -> Beans -> Beans -> Beans
unionBeansMonoidally reps (Beans beans1) (Beans beans2) =
  let d =
        reps
          & Set.map (\v@(SomeMonoidTypeRep tr) -> Data.Semigroup.Arg (SomeTypeRep tr) v)
          & Map.fromArgSet
      combine tr d1 d2 =
        case (Map.lookup tr d, d1, d2) of
          (Just (SomeMonoidTypeRep tr'), Dynamic tr1 v1, Dynamic tr2 v2)
            | Just HRefl <- tr' `eqTypeRep` tr1,
              Just HRefl <- tr' `eqTypeRep` tr2 ->
                toDyn (v1 <> v2)
          _ -> d2
   in Beans $ Map.unionWithKey combine beans1 beans2

keysSet :: Beans -> Set TypeRep
keysSet Beans {beanMap} = Map.keysSet beanMap
