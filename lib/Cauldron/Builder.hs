{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cauldron.Builder
  ( Builder,
    add,
    execBuilder,

    -- * Two beans of the same type are forbidden
    DuplicateBeans (..),
    prettyDuplicateBeans,
    prettyDuplicateBeansLines,

    -- * Being polymorphic on the wiring monad
    MonadWiring (..),
    _ioEff_,
  )
where

import Cauldron
import Cauldron.Args
import Cauldron.Managed
import Control.Exception (Exception (..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor.Identity
import Data.Kind
import Data.List qualified
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

instance MonadFix (Builder m) where
  mfix f =
    let b = f a
        ~(Builder _ _ a) = b
     in b

execBuilder :: Builder m a -> Either DuplicateBeans (Cauldron m)
execBuilder (Builder c m _) =
  let beanDefinitions =
        m
          & Map.mapMaybe \case
            c1 Data.Sequence.:<| c2 Data.Sequence.:<| rest -> Just (c1, c2, Data.Foldable.toList rest)
            _ -> Nothing
   in if (not $ Data.Foldable.null beanDefinitions)
        then Left $ DuplicateBeans beanDefinitions
        else Right c

data DuplicateBeans = DuplicateBeans (Map TypeRep (CallStack, CallStack, [CallStack]))
  deriving stock (Show)

instance Exception DuplicateBeans where
  displayException = prettyDuplicateBeans

prettyDuplicateBeans :: DuplicateBeans -> String
prettyDuplicateBeans = Data.List.intercalate "\n" . prettyDuplicateBeansLines

prettyDuplicateBeansLines :: DuplicateBeans -> [String]
prettyDuplicateBeansLines (DuplicateBeans beanMap) =
  [ "Some bean types defined more than once in builder:"
  ]
    ++ ( beanMap & Map.foldMapWithKey \rep (c1, c2, rest) ->
           ( [ "- Bean type " ++ show rep ++ "was definied in these locations: "
             ]
               ++ ( (c1 : c2 : rest) & foldMap \location ->
                      (("\t" ++) <$> prettyCallStackLines location)
                  )
           )
       )

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

class (Monad m, Applicative (ArgsApplicative m), Monad (ConstructorMonad m)) => MonadWiring m where
  type ArgsApplicative m :: Type -> Type
  type ConstructorMonad m :: Type -> Type
  _val_ :: (Typeable bean, HasCallStack) => ArgsApplicative m bean -> m (ArgsApplicative m bean)
  _eff_ :: (Typeable bean, HasCallStack) => ArgsApplicative m (ConstructorMonad m bean) -> m (ArgsApplicative m bean)

_ioEff_ ::
  (MonadWiring m, MonadIO (ConstructorMonad m), Typeable bean, HasCallStack) =>
  ArgsApplicative m (IO bean) ->
  m (ArgsApplicative m bean)
_ioEff_ args = withFrozenCallStack $ _eff_ $ liftIO <$> args

instance (Monad m) => MonadWiring (Builder m) where
  type ArgsApplicative (Builder m) = Args
  type ConstructorMonad (Builder m) = m
  _val_ :: (Typeable bean, HasCallStack) => Args bean -> Builder m (Args bean)
  _val_ v = withFrozenCallStack $ add (val_ v)
  _eff_ :: (Typeable bean, HasCallStack) => Args (m bean) -> Builder m (Args bean)
  _eff_ action = withFrozenCallStack $ add (eff_ action)

instance MonadWiring IO where
  type ArgsApplicative IO = Identity
  type ConstructorMonad IO = IO
  _val_ :: Identity bean -> IO (Identity bean)
  _val_ = pure
  _eff_ :: Identity (IO bean) -> IO (Identity bean)
  _eff_ = sequence

instance MonadWiring Managed where
  type ArgsApplicative Managed = Identity
  type ConstructorMonad Managed = Managed
  _val_ :: Identity a -> Managed (Identity a)
  _val_ = pure
  _eff_ :: Identity (Managed a) -> Managed (Identity a)
  _eff_ = sequence
