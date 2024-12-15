{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Cauldron.Constructor (runConstructor)
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Dynamic
import Data.Function ((&))
import Data.Functor.Identity
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Typeable (typeRep)
import Test.Tasty
import Test.Tasty.HUnit

data A = A

data B = B

data C = C

makeC :: A -> B -> ([Text], C)
makeC _ _ = (["monoid"], C)

constructorForC :: Constructor Identity C
constructorForC = constructorWithRegs do
  ~(reg1, bean) <- makeC <$> arg <*> arg
  tell1 <- reg
  pure do
    tell1 reg1
    pure bean

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "withRegs" do
        Identity (beans, C) <- pure do runConstructor [[toDyn A, toDyn B]] constructorForC
        Just m <- pure do taste @[Text] beans
        assertEqual
          "monoid"
          ["monoid"]
          m
    ]

main :: IO ()
main = defaultMain tests
