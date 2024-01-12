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

data Weird m = Weird 
  {
    weirdOp :: m (),
    anotherWeirdOp :: m ()
  }

makeWeird :: M (Weird M)
makeWeird = do
  tell ["weird constructor"]
  pure Weird  {
     weirdOp = tell ["weirdOp"],
     anotherWeirdOp  = tell ["another weirdOp"]
    }

-- | Note that the patter-match on the self-dependency must be lazy, or else a
-- nasty, difficult to diagnose infinite loop will happen!
makeSelfInvokingWeird :: Weird M -> M (Weird M)
makeSelfInvokingWeird ~Weird { weirdOp = selfWeirdOp } = do
  tell ["self-invoking weird constructor"]
  pure Weird  {
     weirdOp = tell ["weirdOp 2"],
     anotherWeirdOp  = do
      tell ["another weirdOp 2"]
      selfWeirdOp
    }

weirdDeco :: Text -> Weird M -> Weird M
weirdDeco txt Weird { weirdOp, anotherWeirdOp } =
  Weird {
    weirdOp = do
      tell ["deco for weirdOp " <> txt]
      weirdOp,
    anotherWeirdOp = do
      tell ["deco for anotherWeirdOp " <> txt]
      anotherWeirdOp
  }

cauldron :: Cauldron M
cauldron =
  mempty
    & insert @(Logger M) do makeBean do pack (Packer do fmap (\(reg, bean) -> regs1 reg bean)) do makeLogger
    & insert @(Repository M) do makeBean do pack (Packer do fmap (\(reg, bean) -> regs1 reg bean)) do makeRepository
    & insert @(Initializer, Repository M) do makeBean do pack value do \a b -> (a,b)

cauldronMissingDep :: Cauldron M
cauldronMissingDep = delete @(Logger M) cauldron

cauldronDoubleDutyBean :: Cauldron M
cauldronDoubleDutyBean =
  cauldron
    & insert @Initializer do makeBean do pack value do do (Initializer (pure ()))

cauldronWithCycle :: Cauldron M
cauldronWithCycle =
  cauldron
    & insert @(Logger M) do makeBean do pack (Packer do fmap \(reg, bean) -> regs1 reg bean) do const @_ @(Repository M) makeLogger

cauldronNonEmpty :: NonEmpty (Cauldron M)
cauldronNonEmpty = 
  (mempty
    & do
        let packer = Packer do fmap (\(reg, bean) -> regs1 reg bean)
        insert @(Logger M) do makeBean do pack packer do makeLogger
    & insert @(Weird M) do makeBean do pack effect makeWeird
    )
  Data.List.NonEmpty.:|
  [
    mempty
    & insert @(Repository M) do makeBean do pack (Packer do fmap (\(reg, bean) -> regs1 reg bean)) do makeRepository
    & insert @(Weird M) Bean {
          constructor = pack effect makeSelfInvokingWeird,
          decos = fromConstructors [
               pack value do weirdDeco "inner",
               pack value do weirdDeco "outer"
          ]
        }
    & insert @(Initializer, Repository M, Weird M) do makeBean do pack value do \a b c -> (a,b,c)
  ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ 
      testCase "value" do
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
      testCase "value sequential" do
        (_, traces) <- case cookNonEmpty cauldronNonEmpty of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            _ Data.List.NonEmpty.:| [boiledBeans] <- beansAction
            let (Initializer {runInitializer},
                 Repository {findById, store}, 
                 Weird {anotherWeirdOp}) = fromJust . taste $ boiledBeans
            runInitializer
            store 1 "foo"
            _ <- findById 1
            anotherWeirdOp
            pure ()
        assertEqual
          "traces"
          [ "logger constructor",
            "weird constructor",
            "self-invoking weird constructor",
            "logger init",
            "repo init invoking logger",
            "store",
            "findById",
            -- the deco is applied! The outer the deco, the earliest is invoked.
            "deco for anotherWeirdOp outer",
            "deco for anotherWeirdOp inner",
            "another weirdOp 2",
            "deco for weirdOp outer",
            "deco for weirdOp inner",
            -- note that the self-invocation used the method from 'makeSelfInvokingWeird'
            "weirdOp 2"
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
