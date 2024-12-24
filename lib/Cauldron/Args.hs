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

module Cauldron.Args
  ( getArgsReps,
    getRegsReps,
    Args,
    runArgs,
    LazilyReadBeanMissing (..),
    arg,
    Wireable (wire),
    foretellReg,
    Regs,
    runRegs,
    Registrable (register),

    -- * Re-exports
    Beans,
    taste,
    fromDynList,
    SomeMonoidTypeRep (..),
  )
where

import Cauldron.Beans (Beans, SomeMonoidTypeRep (..), fromDynList, taste)
import Cauldron.Beans qualified
import Control.Exception (Exception, throw)
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Type.Reflection (SomeTypeRep (..))
import Type.Reflection qualified

getArgsReps :: Args a -> Set SomeTypeRep
getArgsReps (Args {_argReps}) = _argReps

getRegsReps :: Args a -> Set SomeMonoidTypeRep
getRegsReps (Args {_regReps}) = _regReps

runArgs :: Args a -> (forall b. (Typeable b) => Maybe b) -> a
runArgs (Args _ _ _runArgs) = _runArgs

data Args a = Args
  { _argReps :: Set SomeTypeRep,
    _regReps :: Set SomeMonoidTypeRep,
    _runArgs :: (forall b. (Typeable b) => Maybe b) -> a
  }
  deriving stock (Functor)

arg :: forall a. (Typeable a) => Args a
arg =
  let tr = typeRep (Proxy @a)
   in Args
        { _argReps = Set.singleton tr,
          _regReps = Set.empty,
          _runArgs = \f ->
            case f @a of
              Just v -> v
              Nothing -> throw (LazilyReadBeanMissing tr)
        }

foretellReg :: forall a. (Typeable a, Monoid a) => Args (a -> Regs ())
foretellReg =
  let tr = SomeMonoidTypeRep (Type.Reflection.typeRep @a)
   in Args
        { _argReps = Set.empty,
          _regReps = Set.singleton tr,
          _runArgs = \_ a -> Regs (Data.Sequence.singleton (toDyn a)) ()
        }

instance Applicative Args where
  pure a =
    Args
      { _argReps = Set.empty,
        _regReps = Set.empty,
        _runArgs = \_ -> a
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
data Regs a = Regs (Seq Dynamic) a
  deriving stock (Functor)

runRegs :: Regs a -> Set SomeMonoidTypeRep -> (Beans, a)
runRegs (Regs dyns a) monoidReps =
  let onlyStaticlyKnown =
        ( manyMemptys monoidReps : do
            dyn <- Data.Foldable.toList dyns
            -- This bit is subtle. I mistakenly used Cauldron.Beans.singleton here
            -- and ended up with the Dynamic type as the *key*. It was hell to debug.
            [fromDynList [dyn]]
        )
          & do foldl (Cauldron.Beans.unionBeansMonoidally monoidReps) (mempty @Beans)
          & do flip Cauldron.Beans.restrictKeys (Set.map someMonoidTypeRepToSomeTypeRep monoidReps)
   in (onlyStaticlyKnown, a)

instance Applicative Regs where
  pure a = Regs Data.Sequence.empty a
  Regs w1 f <*> Regs w2 a2 =
    Regs (w1 Data.Sequence.>< w2) (f a2)

instance Monad Regs where
  (Regs w1 a) >>= k =
    let Regs w2 r = k a
     in Regs (w1 Data.Sequence.>< w2) r

manyMemptys :: Set SomeMonoidTypeRep -> Beans
manyMemptys reps =
  reps
    & Data.Foldable.toList
    <&> someMonoidTypeRepMempty
    & fromDynList

newtype LazilyReadBeanMissing = LazilyReadBeanMissing TypeRep
  deriving stock (Show)
  deriving anyclass (Exception)

class Wireable curried tip | curried -> tip where
  wire :: curried -> Args tip

instance (Wireable_ (IsFunction curried) curried tip) => Wireable curried tip where
  wire curried = wire_ (Proxy @(IsFunction curried)) do pure curried

class Wireable_ (where_ :: Where) curried tip | where_ curried -> tip where
  wire_ :: Proxy where_ -> Args curried -> Args tip

instance Wireable_ AtTheTip a a where
  wire_ _ r = r

instance (Typeable b, Wireable_ (IsFunction rest) rest tip) => Wireable_ NotYetThere (b -> rest) tip where
  wire_ _ af = wire_ (Proxy @(IsFunction rest)) do af <*> arg @b

type IsFunction :: Type -> Where
type family IsFunction f :: Where where
  IsFunction (_ -> _) = 'NotYetThere
  IsFunction _ = 'AtTheTip

data Where
  = NotYetThere
  | AtTheTip

data WhereNested
  = Tup2
  | Tup3
  | Tup4
  | Innermost

type IsReg :: Type -> WhereNested
type family IsReg f :: WhereNested where
  IsReg (_, _) = 'Tup2
  IsReg (_, _, _) = 'Tup3
  IsReg (_, _, _, _) = 'Tup4
  IsReg _ = 'Innermost

class Registrable nested tip | nested -> tip where
  register :: forall m. (Functor m) => Args (m nested) -> Args (m (Regs tip))

instance (Registrable_ (IsReg nested) nested tip) => Registrable nested tip where
  register amnested = register_ (Proxy @(IsReg nested)) do fmap (fmap pure) amnested

class Registrable_ (where_ :: WhereNested) nested tip | where_ nested -> tip where
  register_ :: forall m. (Functor m) => Proxy where_ -> Args (m (Regs nested)) -> Args (m (Regs tip))

instance Registrable_ Innermost a a where
  register_ _ = id

instance (Typeable b, Monoid b, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup2 (b, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      action <- af
      pure (action <&> \regs -> regs >>= \(b, rest) -> tell1 b *> pure rest)

instance (Typeable b, Monoid b, Typeable c, Monoid c, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup3 (b, c, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      tell2 <- foretellReg @c
      action <- af
      pure (action <&> \regs -> regs >>= \(b, c, rest) -> tell1 b *> tell2 c *> pure rest)


instance (Typeable b, Monoid b, Typeable c, Monoid c, Typeable d, Monoid d, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup3 (b, c, d, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      tell2 <- foretellReg @c
      tell3 <- foretellReg @d
      action <- af
      pure (action <&> \regs -> regs >>= \(b, c, d, rest) -> tell1 b *> tell2 c *> tell3 d *> pure rest)