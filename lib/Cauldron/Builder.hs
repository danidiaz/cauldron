{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Cauldron.Builder where

import Cauldron
import Cauldron.Args (Args)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as Graph
import Algebra.Graph.AdjacencyMap.Algorithm qualified as Graph
import Algebra.Graph.Export.Dot qualified as Dot
import Cauldron.Args
import Cauldron.Beans (SomeMonoidTypeRep (..))
import Cauldron.Beans qualified
import Control.Exception (Exception (..))
import Control.Monad.Fix
import Data.Bifunctor (first)
import Data.ByteString qualified
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Kind
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Semigroup qualified
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Tree
import Data.Type.Equality (testEquality)
import Data.Typeable
import GHC.Exception (CallStack, prettyCallStackLines)
import GHC.IsList
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Type.Reflection qualified


data Builder m a = Builder (Cauldron m) a
    deriving stock Functor

instance Applicative (Builder m) where
  pure a = Builder Cauldron.empty a
  Builder c1 f <*> Builder c2 a2 =
    Builder (c1 <> c2) (f a2)

instance Monad (Builder m) where
  (Builder c1 a) >>= k =
    let Builder c2 r = k a
     in Builder (c1 <> c2) r

add ::
  forall {recipelike} {m} (bean :: Type).
  (Typeable bean, ToRecipe recipelike) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  Builder m (Args bean)
add recipelike = Builder (Cauldron.empty & Cauldron.insert  recipelike) (arg @bean)

class MonadBuilder m b | b -> m where
    type ArgsWrapper b :: Type -> Type
    addVal :: Typeable a => ArgsWrapper b a -> m (ArgsWrapper b a)
    addEff :: Typeable a => ArgsWrapper b (b a) -> m (ArgsWrapper b a)

instance (Monad m, Applicative (ArgsWrapper (Builder m))) => MonadBuilder m (Builder m) where
    type ArgsWrapper (Builder m) = Args
    addVal v = add (val_ v) 
    addEff action = add (eff_ action) 