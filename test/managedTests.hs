{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Function ((&))
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Map qualified as Map
import Data.Monoid
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified

newtype Logger m = Logger
  { logMessage :: Text -> m ()
  }

makeLogger :: IORef [Text] -> forall r. (Logger IO -> IO r) -> IO r
makeLogger ref = 
  makeWithWrapperWithMessage ref "allocating logger" "deallocating logger" (
    Logger \message -> 
        modifyIORef ref (++[message])
    )

data Weird m = Weird 
  {
    weirdOp :: m (),
    anotherWeirdOp :: m ()
  }

makeSelfInvokingWeird :: IORef [Text] -> Weird IO -> forall r. (Weird IO -> IO r) -> IO r
makeSelfInvokingWeird ref ~Weird { weirdOp = selfWeirdOp } = do
  makeWithWrapperWithMessage ref "allocating weird" "deallocating weird" (
    Weird  {
     weirdOp = modifyIORef ref (++["weirdOp 2"]),
     anotherWeirdOp  = do
      modifyIORef ref (++["another weirdOp 2"])
      selfWeirdOp
    })

makeWithWrapperWithMessage :: 
    IORef [Text] -> 
    Text -> 
    Text -> 
    a -> forall r. (a -> IO r) -> IO r
makeWithWrapperWithMessage ref inMsg outMsg v handler = do
    modifyIORef ref (++[inMsg])
    r <- handler v
    modifyIORef ref (++[outMsg])
    pure r

managedCauldron :: IORef [Text] -> Cauldron Managed
managedCauldron ref = 
    emptyCauldron
    & insert @(Logger IO) do makeBean do pack effect do managed (makeLogger ref)
    & insert @(Weird IO) do makeBean do pack effect do \self -> managed (makeSelfInvokingWeird ref self)
    & insert @(Logger IO, Weird IO) do makeBean do pack value do (,)

tests :: TestTree
tests =
  testGroup
    "All"
    [    
      testCase "simple" do
        ref <- newIORef []
        case cook allowSelfDeps (managedCauldron ref) of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> with beansAction \boiledBeans -> do
            let (Logger {logMessage}, (Weird {anotherWeirdOp}) :: Weird IO) = fromJust . taste $ boiledBeans
            logMessage "foo"
            -- anotherWeirdOp 
            pure ()
        traces <- readIORef ref
        assertEqual
          "traces"
          [
            "allocating logger",
            "foo",
            "deallocating logger"
          ]
          traces
    ]

main :: IO ()
main = defaultMain tests

