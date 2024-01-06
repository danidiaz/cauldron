{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The example in the executable, as a test.
module Main (main) where

import Cauldron
import Data.Function ((&))
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
makeG _ _ _ = G

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

coolWiring :: Either BadBeans (DependencyGraph, IO (Initializer, Inspector, Z))
coolWiring = do
  let cauldron :: Cauldron IO =
        mempty
          & insert @A do makeBean do pack easy do makeA
          & insert @B do makeBean do pack (pure . \(reg, bean) -> regs1 reg bean) do makeB
          & insert @C do makeBean do pack easy do makeC
          & insert @D do makeBean do pack easy do makeD
          & insert @E do makeBean do pack easy do makeE
          & insert @F do makeBean do pack (pure . \(reg, bean) -> regs1 reg bean) do makeF
          & insert @G
            Bean
              { constructor =  pack easy do makeG,
                decos =
                  fromConstructors
                    [  pack easy do makeGDeco1
                    ]
              }
          & insert @H do makeBean do packPure (\(reg1, reg2, bean) -> regs2 reg1 reg2 bean) do makeH
          & insert @Z
            Bean
              { constructor =  pack easy do makeZ,
                decos =
                  fromConstructors
                    [  pack easy do makeZDeco1,
                      packPure (\(reg, bean) -> regs1 reg bean) do makeZDeco2
                    ]
              }
          & insert @(Initializer, Inspector, Z) do makeBean do  pack easy do \a b c -> (a,b,c)
  fmap (fmap (fmap (fromJust . taste @(Initializer, Inspector, Z)))) do cook cauldron

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
