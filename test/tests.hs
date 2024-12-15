{-# LANGUAGE ApplicativeDo #-}
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
  pure
    ( Initializer do logMessage "repo init invoking logger",
      Repository
        { findById = \key -> do
            logMessage "findById"
            m <- liftIO do readIORef mapRef
            pure do Map.lookup key m,
          store = \key v -> do
            logMessage "store"
            liftIO do modifyIORef mapRef do Map.insert key v
        }
    )

data Weird m = Weird
  { weirdOp :: m (),
    anotherWeirdOp :: m ()
  }

makeWeird :: Logger M -> M (Weird M)
makeWeird _ = do
  tell ["weird constructor"]
  pure
    Weird
      { weirdOp = tell ["weirdOp"],
        anotherWeirdOp = tell ["another weirdOp"]
      }

data Lonely m = Lonely {soLonely :: m ()}

makeLonely :: Lonely M
makeLonely = do
  Lonely {soLonely = tell ["so lonely"]}

-- | Note that the patter-match on the self-dependency must be lazy, or else a
-- nasty, difficult to diagnose infinite loop will happen!
makeSelfInvokingWeird :: Weird M -> M (Weird M)
makeSelfInvokingWeird ~Weird {weirdOp = selfWeirdOp} = do
  tell ["self-invoking weird constructor"]
  pure
    Weird
      { weirdOp = tell ["weirdOp 2"],
        anotherWeirdOp = do
          tell ["another weirdOp 2"]
          selfWeirdOp
      }

weirdDeco :: Text -> Weird M -> Weird M
weirdDeco txt Weird {weirdOp, anotherWeirdOp} =
  Weird
    { weirdOp = do
        tell ["deco for weirdOp " <> txt]
        weirdOp,
      anotherWeirdOp = do
        tell ["deco for anotherWeirdOp " <> txt]
        anotherWeirdOp
    }

cauldron :: Cauldron M
cauldron =
  fromSomeRecipeList
    [ someRecipe @(Logger M) $ effectfulConstructorWithRegs do
        action <- pure makeLogger
        tell1 <- reg
        pure do
          (reg1, bean) <- action
          pure do
            tell1 reg1
            pure bean,
      someRecipe @(Repository M) $ effectfulConstructorWithRegs do
        action <- makeRepository <$> arg
        tell1 <- reg
        pure do
          (reg1, bean) <- action
          pure do
            tell1 reg1
            pure bean,
      someRecipe @(Initializer, Repository M) $ constructor do fillArgs (,)
    ]

cauldronMissingDep :: Cauldron M
cauldronMissingDep =
  cauldron
    & delete (typeRep (Proxy @(Logger M)))

cauldronDoubleDutyBean :: Cauldron M
cauldronDoubleDutyBean =
  cauldron
    & insert @Initializer (constructor do pure (Initializer (pure ())))

cauldronWithCycle :: Cauldron M
cauldronWithCycle =
  cauldron
    & insert @(Logger M)
      ( effectfulConstructorWithRegs do
          action <- fillArgs \(_ :: Repository M) -> makeLogger
          tell1 <- reg
          pure do
            (reg1, bean) <- action
            pure do
              tell1 reg1
              pure bean
      )

cauldronNonEmpty :: NonEmpty (Cauldron M)
cauldronNonEmpty =
  Data.List.NonEmpty.fromList
    [ fromSomeRecipeList
        [ someRecipe @(Logger M) $ effectfulConstructorWithRegs1 do pure makeLogger,
          someRecipe @(Weird M) $ effectfulConstructor do fillArgs makeWeird
        ],
      fromSomeRecipeList
        [ someRecipe @(Repository M) $ effectfulConstructorWithRegs1 do fillArgs makeRepository,
          someRecipe @(Weird M)
            Recipe
              { bean = effectfulConstructor do fillArgs makeSelfInvokingWeird,
                decos =
                  [ constructor do fillArgs (weirdDeco "inner"),
                    constructor do fillArgs (weirdDeco "outer")
                  ]
              },
          someRecipe @(Initializer, Repository M, Weird M) $ constructor do fillArgs (,,)
        ]
    ]

cauldronLonely :: Cauldron M
cauldronLonely =
  fromSomeRecipeList
    [ someRecipe @(Lonely M) $ constructor do pure makeLonely
    ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "value" do
        (_, traces) <- case cook' cauldron of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            boiledBeans <- beansAction
            let (Initializer {runInitializer}, Repository {findById, store}) = fromJust . taste $ boiledBeans
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
        (_, traces) <- case cookNonEmpty' cauldronNonEmpty of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            _ Data.List.NonEmpty.:| [boiledBeans] <- beansAction
            let ( Initializer {runInitializer},
                  Repository {findById, store},
                  Weird {anotherWeirdOp}
                  ) = fromJust . taste $ boiledBeans
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
      testCase "lonely beans get built" do
        (_, _) <- case cook' cauldronLonely of
          Left _ -> assertFailure "could not wire"
          Right (_, beansAction) -> runWriterT do
            boiledBeans <- beansAction
            let Lonely {soLonely} = fromJust . taste $ boiledBeans
            soLonely
            pure ()
        pure (),
      testCase "cauldron missing dep" do
        case cook' cauldronMissingDep of
          Left (MissingDependencies [] missingMap)
            | Map.size missingMap == 1 -> pure ()
            | otherwise -> assertFailure "missing dependency error has too many entries"
          _ -> assertFailure "missing dependency not detected"
        pure (),
      testCase "cauldron with double duty bean" do
        case cook' cauldronDoubleDutyBean of
          Left (DoubleDutyBeans _) -> pure ()
          _ -> assertFailure "double duty beans not detected"
        pure (),
      testCase "cauldron with cycle" do
        case cook' cauldronWithCycle of
          Left (DependencyCycle _) -> pure ()
          _ -> assertFailure "dependency cycle not detected"
        pure ()
    ]
  where
    cook' = cook allowSelfDeps
    cookNonEmpty' = cookNonEmpty . fmap (allowSelfDeps,)

main :: IO ()
main = defaultMain tests
