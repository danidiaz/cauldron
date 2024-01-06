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

type M = WriterT [Text] IO

-- | And initialization action which some beans might register.
newtype Initializer = Initializer {runInitializer :: M ()}
  deriving (Semigroup, Monoid) via (Ap M ())

newtype Logger m = Logger
  { logMessage :: Text -> m ()
  }

makeLogger :: M (Initializer, Logger M)
makeLogger = do
  tell ["logger constructor"]
  pure
    ( Initializer do tell ["logger init"],
      Logger \message -> tell [message]
    )

data Repository m = Repository
  { findById :: Int -> m (Maybe Text),
    store :: Int -> Text -> m ()
  }

makeRepository :: Logger M -> M (Initializer, Repository M)
makeRepository Logger {logMessage} = do
    mapRef <- liftIO do newIORef @(Map Int Text) mempty
    pure ( 
      Initializer do logMessage "repo init invoking logger",
      Repository
        { findById = \key -> do
            logMessage "findById"
            m <- liftIO do readIORef mapRef
            pure do Map.lookup key m,
          store = \key value -> do
            logMessage "store"
            liftIO do modifyIORef mapRef do Map.insert key value
        }
      )

cauldron :: Cauldron M
cauldron =
  emptyCauldron
    & insert @(Logger M) do makeBean do pack (fmap (\(reg, bean) -> regs1 reg bean)) do makeLogger
    & insert @(Repository M) do makeBean do pack (fmap (\(reg, bean) -> regs1 reg bean)) do makeRepository
    & insert @(Initializer, Repository M) do makeBean do packPure regs0 do \a b -> (a,b)

cauldronMissingDep :: Cauldron M
cauldronMissingDep = delete @(Logger M) cauldron

cauldronDoubleDutyBean :: Cauldron M
cauldronDoubleDutyBean =
  cauldron
    & insert @Initializer do makeBean do packPure regs0 do do (Initializer (pure ()))

cauldronWithCycle :: Cauldron M
cauldronWithCycle =
  cauldron
    & insert @(Logger M) do makeBean do pack (fmap \(reg, bean) -> regs1 reg bean) do const @_ @(Repository M) makeLogger

cauldronNonEmpty :: NonEmpty (Cauldron M)
cauldronNonEmpty = 
  (emptyCauldron
    & insert @(Logger M) do makeBean do pack (fmap (\(reg, bean) -> regs1 reg bean)) do makeLogger)
  Data.List.NonEmpty.:|
  [
    emptyCauldron
    & insert @(Repository M) do makeBean do pack (fmap (\(reg, bean) -> regs1 reg bean)) do makeRepository
    & insert @(Initializer, Repository M) do makeBean do packPure regs0 do \a b -> (a,b)
  ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ 
      testCase "simple" do
        (_, traces) <- case cook cauldron of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            boiledBeans <- beansAction 
            let (Initializer {runInitializer},Repository {findById, store}) = fromJust . taste $ boiledBeans
            runInitializer
            store 1 "foo"
            _ <- findById 1
            pure ()
        assertEqual
          "traces"
          [ "logger constructor",
            "logger init",
            "repo init invoking logger",
            "store",
            "findById"
          ]
          traces,
      testCase "simple sequential" do
        (_, traces) <- case cookNonEmpty cauldronNonEmpty of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            _ Data.List.NonEmpty.:| [boiledBeans] <- beansAction
            let (Initializer {runInitializer},Repository {findById, store}) = fromJust . taste $ boiledBeans
            runInitializer
            store 1 "foo"
            _ <- findById 1
            pure ()
        assertEqual
          "traces"
          [ "logger constructor",
            "logger init",
            "repo init invoking logger",
            "store",
            "findById"
          ]
          traces,
      testCase "cauldron missing dep" do
        case cook cauldronMissingDep of
          Left (MissingDependencies _) -> pure ()
          _ -> assertFailure "missing dependency not detected"
        pure (),
      testCase "cauldron with double duty bean" do
        case cook cauldronDoubleDutyBean of
          Left (DoubleDutyBeans _) -> pure ()
          _ -> assertFailure "double duty beans not detected"
        pure (),
      testCase "cauldron with cycle" do
        case cook cauldronWithCycle of
          Left (DependencyCycle _) -> pure ()
          _ -> assertFailure "dependency cycle not detected"
        pure ()
    ]

main :: IO ()
main = defaultMain tests
