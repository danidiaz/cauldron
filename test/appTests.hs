{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The example in the executable, as a test.
module Main (main) where

import Cauldron
import Data.Function ((&))
import Data.Monoid
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

makeE :: IO (A -> E)
makeE = pure \_ -> E

makeF :: IO (B -> C -> (Inspector, F))
makeF = pure \_ _ -> (Inspector (pure ["F stuff"]), F)

makeG :: E -> F -> G
makeG _ _ = G

makeGDeco1 :: A -> G -> G
makeGDeco1 _ g = g 

makeH :: A -> D -> G -> (Initializer, Inspector, H)
makeH _ _ _ = (Initializer (putStrLn "H init"), Inspector (pure ["H stuff"]), H)

makeZ :: Inspector -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Z -> Z
makeZDeco1 _ _ z = z

makeZDeco2 :: IO (F -> Z -> (Initializer, Z))
makeZDeco2 = pure \_ z -> (Initializer (putStrLn "Z deco init"), z)

coolWiring :: Either BadBeans (DependencyGraph, IO (Maybe (Initializer, Inspector, Z)))
coolWiring =
  let cauldron :: Cauldron IO =
        empty
          & insert @A do bare do pack_ do pure makeA
          & insert @B do bare do pack (\(reg, bean) -> regs1 reg bean) do pure makeB
          & insert @C do bare do pack_ do pure makeC
          & insert @D do bare do pack_ do pure makeD
          & insert @E do bare do pack_ do makeE
          & insert @F do bare do pack (\(reg, bean) -> regs1 reg bean) do makeF
          & insert @G do
            Bean
              { constructor = pack_ do pure makeG,
                decos =
                  fromConstructors
                    [ pack_ do pure makeGDeco1
                    ]
              }
          & insert @H do bare do pack (\(reg1, reg2, bean) -> regs2 reg1 reg2 bean) do pure makeH
          & insert @Z do
            Bean
              { constructor = pack_ do pure makeZ,
                decos =
                  fromConstructors
                    [ pack_ do pure makeZDeco1,
                      pack (\(reg, bean) -> regs1 reg bean) do makeZDeco2
                    ]
              }
   in case cook cauldron of
        Left e -> Left e
        Right (depGraph, action) ->
          Right
            ( depGraph,
              do
                beans <- action
                pure do
                  initializer <- taste @Initializer beans
                  inspector <- taste @Inspector beans
                  z <- taste @Z beans
                  pure (initializer, inspector, z)
            )

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "example" do
        case coolWiring of
          Left badBeans -> assertFailure do show badBeans
          Right _ -> pure ()
        pure ()
    ]

main :: IO ()
main = defaultMain tests
