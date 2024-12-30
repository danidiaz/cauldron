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

data Builder m a = Builder (Cauldron m) a
  deriving stock (Functor)

instance Applicative (Builder m) where
  pure a = Builder Cauldron.empty a
  Builder c1 f <*> Builder c2 a2 =
    Builder (c1 <> c2) (f a2)

instance Monad (Builder m) where
  (Builder c1 a) >>= k =
    let Builder c2 r = k a
     in Builder (c1 <> c2) r

execBuilder :: Builder m a -> Cauldron m
execBuilder (Builder c _) = c

add ::
  forall {recipelike} {m} (bean :: Type).
  (Typeable bean, ToRecipe recipelike) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  Builder m (Args bean)
add recipelike = Builder (Cauldron.empty & Cauldron.insert recipelike) (arg @bean)

class (Monad m, Applicative (ArgsApplicative m), Monad (ConstructorMonad m)) => MonadBuilder m where
  type ArgsApplicative m :: Type -> Type
  type ConstructorMonad m :: Type -> Type
  addVal_ :: (Typeable bean) => ArgsApplicative m bean -> m (ArgsApplicative m bean)
  addEff_ :: (Typeable bean) => ArgsApplicative m (ConstructorMonad m bean) -> m (ArgsApplicative m bean)

addIOEff_ ::
  (MonadBuilder m, MonadIO (ConstructorMonad m), Typeable bean) =>
  ArgsApplicative m (IO bean) ->
  m (ArgsApplicative m bean)
addIOEff_ args = addEff_ $ liftIO <$> args

instance (Monad m) => MonadBuilder (Builder m) where
  type ArgsApplicative (Builder m) = Args
  type ConstructorMonad (Builder m) = m
  addVal_ :: (Typeable bean) => Args bean -> Builder m (Args bean)
  addVal_ v = add (val_ v)
  addEff_ :: (Typeable bean) => Args (m bean) -> Builder m (Args bean)
  addEff_ action = add (eff_ action)

instance MonadBuilder IO where
  type ArgsApplicative IO = Identity
  type ConstructorMonad IO = IO
  addVal_ :: (Typeable bean) => Identity bean -> IO (Identity bean)
  addVal_ = pure
  addEff_ :: (Typeable bean) => Identity (IO bean) -> IO (Identity bean)
  addEff_ = sequence

instance MonadBuilder Managed where
  type ArgsApplicative Managed = Identity
  type ConstructorMonad Managed = Managed
  addVal_ :: (Typeable a) => Identity a -> Managed (Identity a)
  addVal_ = pure
  addEff_ :: (Typeable a) => Identity (Managed a) -> Managed (Identity a)
  addEff_ = sequence