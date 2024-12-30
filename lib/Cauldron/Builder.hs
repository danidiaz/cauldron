{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cauldron.Builder
  ( Builder,
    execBuilder,
    add,
    MonadBuilder (..),
  )
where

import Cauldron
import Cauldron.Args
import Cauldron.Managed
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

class (Monad b, Applicative (ArgsWrapper b), Monad (ConstructorEff b)) => MonadBuilder b where
  type ArgsWrapper b :: Type -> Type
  type ConstructorEff b :: Type -> Type
  addVal :: (Typeable a) => ArgsWrapper b a -> b (ArgsWrapper b a)
  addEff :: (Typeable a) => ArgsWrapper b (ConstructorEff b a) -> b (ArgsWrapper b a)

instance (Monad m) => MonadBuilder (Builder m) where
  type ArgsWrapper (Builder m) = Args
  type ConstructorEff (Builder m) = m
  addVal :: (Monad m, Typeable a) => Args a -> Builder m (Args a)
  addVal v = add (val_ v)
  addEff :: (Monad m, Typeable a) => Args (m a) -> Builder m (Args a)
  addEff action = add (eff_ action)

instance MonadBuilder IO where
  type ArgsWrapper IO = Identity
  type ConstructorEff IO = IO
  addVal :: (Typeable a) => Identity a -> IO (Identity a)
  addVal = pure
  addEff :: (Typeable a) => Identity (IO a) -> IO (Identity a)
  addEff = sequence

instance MonadBuilder Managed where
  type ArgsWrapper Managed = Identity
  type ConstructorEff Managed = Managed
  addVal :: (Typeable a) => Identity a -> Managed (Identity a)
  addVal = pure
  addEff :: (Typeable a) => Identity (Managed a) -> Managed (Identity a)
  addEff = sequence
