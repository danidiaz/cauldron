{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cauldron.Builder
  ( Builder,
    execBuilder,
    add,
    MonadBuilder (..),
    addIOEff_,
  )
where

import Cauldron
import Cauldron.Args
import Cauldron.Managed
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Function ((&))
import Data.Functor.Identity
import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Typeable
import GHC.Exception (CallStack, prettyCallStackLines)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

data Builder m a = Builder (Cauldron m) (Map TypeRep (Seq CallStack)) a
  deriving stock (Functor)

combineCallStackMaps :: Map TypeRep (Seq CallStack) -> Map TypeRep (Seq CallStack) -> Map TypeRep (Seq CallStack)
combineCallStackMaps = Map.unionWith (Data.Sequence.><)

instance Applicative (Builder m) where
  pure a = Builder Cauldron.empty Map.empty a
  Builder c1 m1 f <*> Builder c2 m2 a2 =
    Builder (c1 <> c2) (combineCallStackMaps m1 m2) (f a2)

instance Monad (Builder m) where
  (Builder c1 m1 a) >>= k =
    let Builder c2 m2 r = k a
     in Builder (c1 <> c2) (combineCallStackMaps m1 m2) r

execBuilder :: Builder m a -> Cauldron m
execBuilder (Builder c _ _) = c

add ::
  forall {recipelike} {m} (bean :: Type).
  (Typeable bean, ToRecipe recipelike, HasCallStack) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  Builder m (Args bean)
add recipelike =
  Builder
    (Cauldron.empty & Cauldron.insert recipelike)
    (Map.singleton (typeRep (Proxy @bean)) (Data.Sequence.singleton callStack))
    (arg @bean)

class (Monad m, Applicative (ArgsApplicative m), Monad (ConstructorMonad m)) => MonadBuilder m where
  type ArgsApplicative m :: Type -> Type
  type ConstructorMonad m :: Type -> Type
  addVal_ :: (Typeable bean, HasCallStack) => ArgsApplicative m bean -> m (ArgsApplicative m bean)
  addEff_ :: (Typeable bean, HasCallStack) => ArgsApplicative m (ConstructorMonad m bean) -> m (ArgsApplicative m bean)

addIOEff_ ::
  (MonadBuilder m, MonadIO (ConstructorMonad m), Typeable bean, HasCallStack) =>
  ArgsApplicative m (IO bean) ->
  m (ArgsApplicative m bean)
addIOEff_ args = withFrozenCallStack $ addEff_ $ liftIO <$> args

instance (Monad m) => MonadBuilder (Builder m) where
  type ArgsApplicative (Builder m) = Args
  type ConstructorMonad (Builder m) = m
  addVal_ :: (Typeable bean, HasCallStack) => Args bean -> Builder m (Args bean)
  addVal_ v = withFrozenCallStack $ add (val_ v)
  addEff_ :: (Typeable bean, HasCallStack) => Args (m bean) -> Builder m (Args bean)
  addEff_ action = withFrozenCallStack $ add (eff_ action)

instance MonadBuilder IO where
  type ArgsApplicative IO = Identity
  type ConstructorMonad IO = IO
  addVal_ :: Identity bean -> IO (Identity bean)
  addVal_ = pure
  addEff_ :: Identity (IO bean) -> IO (Identity bean)
  addEff_ = sequence

instance MonadBuilder Managed where
  type ArgsApplicative Managed = Identity
  type ConstructorMonad Managed = Managed
  addVal_ :: Identity a -> Managed (Identity a)
  addVal_ = pure
  addEff_ :: Identity (Managed a) -> Managed (Identity a)
  addEff_ = sequence
