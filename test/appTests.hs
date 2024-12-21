{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The example in the executable, as a test.
module Main (main) where

import Cauldron
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

data A = A deriving (Show)

data B = B deriving (Show)

data C = C deriving (Show)

data D = D deriving (Show)

data E = E deriving (Show)

data F = F deriving (Show)

data G = G deriving (Show)

data H = H deriving (Show)

data I = I deriving (Show)

data J = J deriving (Show)

data K = K deriving (Show)

data L = L deriving (Show)

data M = M deriving (Show)

data N = N deriving (Show)

data O = O deriving (Show)

data P = P deriving (Show)

data Q = Q deriving (Show)

data R = R deriving (Show)

data S = S deriving (Show)

data T = T deriving (Show)

data U = U deriving (Show)

data V = V deriving (Show)

data W = W deriving (Show)

data X = X deriving (Show)

data Y = Y deriving (Show)

data Z = Z deriving (Show)

newtype Inspector = Inspector {inspect :: IO [String]}
  deriving newtype (Semigroup, Monoid)

newtype Initializer = Initializer {runInitializer :: IO ()}
  deriving newtype (Semigroup, Monoid)

makeA :: A
makeA = A

makeB :: (Inspector, B)
makeB = (Inspector (pure ["B stuff"]), B)

makeC :: C
makeC = C

makeD :: D
makeD = D

makeE :: A -> E
makeE = \_ -> E

makeF :: B -> C -> (Inspector, F)
makeF = \_ _ -> (Inspector (pure ["F stuff"]), F)

makeG :: E -> F -> G -> G
makeG _ _ (_ :: G) = G

makeGDeco1 :: A -> G -> G
makeGDeco1 _ g = g

makeH :: A -> D -> G -> (Initializer, Inspector, H)
makeH _ _ _ = (Initializer (putStrLn "H init"), Inspector (pure ["H stuff"]), H)

makeZ :: Inspector -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Z -> Z
makeZDeco1 _ _ z = z

makeZDeco2 :: F -> Z -> (Initializer, Z)
makeZDeco2 = \_ z -> (Initializer (putStrLn "Z deco init"), z)

coolWiring :: Fire IO -> Either RecipeError (DependencyGraph, IO (Initializer, Inspector, Z))
coolWiring fire = do
  let cauldron :: Cauldron IO =
        fromRecipeList
          [ recipe $ val do pure makeA,
            recipe $ val do pure makeB,
            recipe $ val do wire makeC,
            recipe $ val do wire makeD,
            recipe $ val do wire makeE,
            recipe @F $ val do wire makeF,
            recipe @G
              Recipe
                { bean = val do wire makeG,
                  decos =
                    fromDecoList
                      [ val do wire makeGDeco1
                      ]
                },
            recipe @H $ val do wire makeH,
            recipe @Z
              Recipe
                { bean = val do wire makeZ,
                  decos =
                    fromDecoList
                      [ val do wire makeZDeco1,
                        val do wire makeZDeco2
                      ]
                },
            recipe @(Initializer, Inspector, Z) $ val0 do wire (,,)
          ]
  fmap (fmap (fmap (fromJust . taste @(Initializer, Inspector, Z)))) do cook fire cauldron

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "example" do
        case coolWiring allowSelfDeps of
          Left badBeans -> assertFailure do show badBeans
          Right _ -> pure ()
        pure (),
      testCase "dep cycles forbidden" do
        case coolWiring forbidDepCycles of
          Left (DependencyCycle _) -> pure ()
          Left _ -> assertFailure do "wrong kind of error detected"
          Right _ -> assertFailure do "self dependency not detected"
        pure ()
    ]

main :: IO ()
main = defaultMain tests
