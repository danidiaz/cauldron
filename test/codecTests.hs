{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Data.Foldable qualified
import Data.Functor.Identity
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit

data Foo
  = EndFoo
  | FooToBar Bar
  deriving stock (Show)

data Bar
  = EndBar
  | BarToFoo Foo
  | BarToBaz Baz
  deriving stock (Show)

data Baz
  = EndBaz
  | BazToFoo Foo
  deriving stock (Show)

newtype Serializer a = Serializer {runSerializer :: a -> String}

makeFooSerializer :: Serializer Bar -> Serializer Foo
makeFooSerializer Serializer {runSerializer = runBar} =
  Serializer
    { runSerializer = \case
        EndFoo -> ".EndFoo"
        FooToBar bar -> ".FooToBar" ++ runBar bar
    }

makeBarSerializer :: Serializer Foo -> Serializer Baz -> Serializer Bar
makeBarSerializer Serializer {runSerializer = runFoo} Serializer {runSerializer = runBaz} =
  Serializer
    { runSerializer = \case
        EndBar -> ".EndBar"
        BarToFoo foo -> ".BarToFoo" ++ runFoo foo
        BarToBaz baz -> ".BarToBar" ++ runBaz baz
    }

makeBazSerializer :: Serializer Foo -> Serializer Baz
makeBazSerializer Serializer {runSerializer = runFoo} =
  Serializer
    { runSerializer = \case
        EndBaz -> ".EndBaz"
        BazToFoo foo -> ".BazToFoo" ++ runFoo foo
    }

cauldron :: Cauldron Identity
cauldron =
  fromRecipeList
    [ recipe @(Serializer Foo) $ val $ wire makeFooSerializer,
      recipe @(Serializer Bar) $ val $ wire makeBarSerializer,
      recipe @(Serializer Baz) $ val $ wire makeBazSerializer
    ]

newtype Acc = Acc Int
  deriving stock (Show)
  deriving stock (Eq)
  deriving (Semigroup, Monoid) via Sum Int

newtype Bcc = Bcc Int
  deriving stock (Show)
  deriving stock (Eq)
  deriving (Semigroup, Monoid) via Sum Int

cauldronAccums1 :: Cauldron Identity
cauldronAccums1 =
  fromRecipeList
    [ recipe @(Serializer Foo) $ val $ wire $ \sb -> (Acc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccums2 :: Cauldron Identity
cauldronAccums2 =
  fromRecipeList
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> makeFooSerializer sb,
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccumsOops1 :: Cauldron Identity
cauldronAccumsOops1 =
  fromRecipeList
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> (Acc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccumsOops2 :: Cauldron Identity
cauldronAccumsOops2 =
  fromRecipeList
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> (Bcc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \(_ :: Bcc) sf sb -> (Acc 5, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "successful cyclic wiring" do
        case cook allowDepCycles cauldron of
          Left _ -> do
            -- putStrLn $ prettyRecipeError err
            assertFailure "could not wire"
          Right (_, Identity bs) ->
            case taste bs of
              Nothing -> assertFailure "serializer not found"
              Just (Serializer {runSerializer}) -> do
                let value = FooToBar (BarToFoo (FooToBar (BarToBaz EndBaz)))
                assertEqual "experted result" ".FooToBar.BarToFoo.FooToBar.BarToBar.EndBaz" (runSerializer value),
      testCase "should fail cycle wiring" do
        Data.Foldable.for_ @[] [("forbid", forbidDepCycles), ("selfdeps", allowSelfDeps)] \(name, fire) ->
          case cook fire cauldron of
            Left (DependencyCycleError _) -> pure ()
            Left _ -> assertFailure $ "Unexpected error when wiring" ++ name
            Right (_, _) -> assertFailure $ "Unexpected success when wiring" ++ name,
      testCase "cyclic wiring with accums" do
        Data.Foldable.for_ @[]
          [ ("normal", cauldronAccums1, Acc 15),
            ("someConsume", cauldronAccums2, Acc 10)
          ]
          \(name, c, expected) ->
            case cook allowDepCycles c of
              Left _err -> do
                -- putStrLn $ prettyRecipeError err
                assertFailure $ "could not wire " ++ name
              Right (_, Identity bs) ->
                case taste bs of
                  Nothing -> assertFailure $ "accum not found " ++ name
                  Just (acc :: Acc) -> do
                    assertEqual "experted result" expected acc,
      testCase "problematic wiring with accums" do
        Data.Foldable.for_ @[]
          [ ("selfacc", cauldronAccumsOops1),
            ("indirectacc", cauldronAccumsOops2)
          ]
          \(name, c) ->
            case cook allowDepCycles c of
              Left (DependencyCycleError _) -> pure ()
              Left _ -> assertFailure $ "Unexpected error when wiring" ++ name
              Right (_, _) -> assertFailure $ "Unexpected success when wiring" ++ name
    ]

main :: IO ()
main = defaultMain tests
