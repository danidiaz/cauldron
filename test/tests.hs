{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Function ((&))
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified
import Test.Tasty
import Test.Tasty.HUnit

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

makeRepository :: IO (Logger M -> (Initializer, Repository M))
makeRepository = do
  mapRef <- newIORef @(Map Int Text) mempty
  pure \Logger {logMessage} ->
    ( Initializer do logMessage "repo init invoking logger",
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
  empty
    & insert @(Logger M) do bare do pack (\(reg, bean) -> regs1 reg bean) do makeLogger
    & insert @(Repository M) do bare do pack (\(reg, bean) -> regs1 reg bean) do lift makeRepository

cauldronMissingDep :: Cauldron M
cauldronMissingDep = delete @(Logger M) cauldron

cauldronDoubleDutyBean :: Cauldron M
cauldronDoubleDutyBean =
  cauldron
    & insert @Initializer do bare do pack_ do pure do (Initializer (pure ()))

cauldronWithCycle :: Cauldron M
cauldronWithCycle =
  cauldron
    & insert @(Logger M) do bare do pack (\(reg, bean) -> regs1 reg bean) do const @_ @(Repository M) <$> makeLogger

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "simple" do
        (_, traces) <- case cook cauldron of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            beans <- beansAction
            case ( liftA2
                     (,)
                     (taste @Initializer beans)
                     (taste @(Repository M) beans)
                 ) of
              Nothing -> liftIO do assertFailure "bean not built"
              Just
                ( Initializer {runInitializer},
                  Repository {findById, store}
                  ) -> do
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
