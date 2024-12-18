{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  ( 
    getArgReps,
    getRegReps,
    Args,
    runArgs,
    arg,
    fillArgs,
    reg,
    Regs,
    runRegs,
    -- * Re-exports
    Beans,
    taste,
    fromDynList,
    SomeMonoidTypeRep (..),
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Cauldron.Beans (Beans, SomeMonoidTypeRep (..), fromDynList, taste)
import Cauldron.Beans qualified
import Control.Applicative
import Control.Exception (Exception, throw)
import Control.Monad.Fix
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
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
import Data.Semigroup qualified
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


getArgReps :: Args a -> Set SomeTypeRep
getArgReps (Args {_argReps}) = _argReps

getRegReps :: Args a -> Set SomeMonoidTypeRep
getRegReps (Args {_regReps}) = _regReps

runArgs :: Args a -> [Beans] -> a
runArgs (Args _ _ _runArgs) = _runArgs

data Args a = Args
  { _argReps :: Set SomeTypeRep,
    _regReps :: Set SomeMonoidTypeRep,
    _runArgs :: [Beans] -> a
  }
  deriving stock (Functor)

arg :: forall a. (Typeable a) => Args a
arg =
  let tr = typeRep (Proxy @a)
   in Args
        { _argReps = Set.singleton tr,
          _regReps = Set.empty,
          _runArgs = \bss ->
            case asum do taste <$> bss of
              Just v -> v
              Nothing -> throw (LazilyReadBeanMissing tr)
        }

reg :: forall a. (Typeable a, Monoid a) => Args (a -> Regs ())
reg =
  let tr = SomeMonoidTypeRep (Type.Reflection.typeRep @a)
   in Args
        { _argReps = Set.empty,
          _regReps = Set.singleton tr,
          _runArgs = pure \a -> Regs [toDyn a] ()
        }

instance Applicative Args where
  pure a =
    Args
      { _argReps = Set.empty,
        _regReps = Set.empty,
        _runArgs = pure a
      }
  Args
    { _argReps = _argReps1,
      _regReps = _regReps1,
      _runArgs = f
    }
    <*> Args
      { _argReps = _argReps2,
        _regReps = _regReps2,
        _runArgs = a
      } =
      Args
        { _argReps = _argReps1 `Set.union` _argReps2,
          _regReps = _regReps1 `Set.union` _regReps2,
          _runArgs = \beans -> (f beans) (a beans)
        }

someMonoidTypeRepMempty :: SomeMonoidTypeRep -> Dynamic
someMonoidTypeRepMempty (SomeMonoidTypeRep @t _) = toDyn (mempty @t)

someMonoidTypeRepToSomeTypeRep :: SomeMonoidTypeRep -> SomeTypeRep
someMonoidTypeRepToSomeTypeRep (SomeMonoidTypeRep tr) = SomeTypeRep tr

-- | Unrestricted building SHOULD NOT be public!
data Regs a = Regs [Dynamic] a
  deriving stock (Functor)

runRegs :: Regs a -> Set SomeMonoidTypeRep -> (Beans, a)
runRegs (Regs dyns a) monoidReps = 
  let onlyStaticlyKnown =
        ( manyMemptys monoidReps : do
            dyn <- dyns
            -- This bit is subtle. I mistakenly used Cauldron.Beans.singleton here
            -- and ended up with the Dynamic type as the *key*. It was hell to debug.
            [fromDynList [dyn]]
        )
          & do foldl (Cauldron.Beans.unionBeansMonoidally monoidReps) (mempty @Beans)
          & do flip Cauldron.Beans.restrict (Set.map someMonoidTypeRepToSomeTypeRep monoidReps)
   in (onlyStaticlyKnown, a)

instance Applicative Regs where
  pure a = Regs [] a
  Regs w1 f <*> Regs w2 a2 =
    Regs (w1 ++ w2) (f a2)

instance Monad Regs where
  (Regs w1 a) >>= k =
    let Regs w2 r = k a
     in Regs (w1 ++ w2) r

fillArgs ::
  forall (args :: [Type]) r curried.
  ( All Typeable args,
    (MulticurryableF args r curried (IsFunction curried))
  ) =>
  curried ->
  Args r
fillArgs curried =
  let uncurried = multiuncurry curried
      args = cpure_NP (Proxy @Typeable) arg
      sequencedArgs = sequence_NP args
      _argReps = cfoldMap_NP (Proxy @Typeable) (Set.singleton . typeRep) args
   in uncurried <$> sequencedArgs <* Args {_argReps, _regReps = mempty, _runArgs = \_ -> Right ()}

manyMemptys :: Set SomeMonoidTypeRep -> Beans
manyMemptys reps =
  reps
    & Data.Foldable.toList
    <&> someMonoidTypeRepMempty
    & fromDynList

newtype LazilyReadBeanMissing = LazilyReadBeanMissing TypeRep
  deriving stock (Show)
  deriving anyclass (Exception)
