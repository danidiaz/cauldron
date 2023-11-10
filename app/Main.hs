{-# LANGUAGE BlockArguments #-}
-- | We have a bunch of datatypes, and a single recipe (constructor) for each
-- datatype. This means the wiring can be type-directed: we don't have to make a
-- decision at the term level about which constructor to use. 
--
-- We wire the constructors in two ways: manually, and using a bit of dynamic
-- typing magic from the "Cauldron" module.
module Main where

import Cauldron 
import Cauldron qualified
import Data.Proxy
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Monoid

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

makeA :: (Sum Int, A)
makeA = (Sum 1, A)

makeB :: B
makeB = B

makeC :: C
makeC = C

makeD :: D
makeD = D

makeE :: A -> E
makeE _ = E

makeF :: B -> C -> F
makeF _ _ = F

makeG :: E -> F -> G -> G -- self-dependency!
makeG _ _ _ = G

makeGDeco1 :: A -> Endo G
makeGDeco1 _ = mempty 

makeH :: A -> D -> G -> (Sum Int, H)
makeH _ _ _ = (Sum 1, H)

makeZ :: Sum Int -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Endo Z
makeZDeco1 _ _ = mempty

makeZDeco2 :: F -> Endo Z
makeZDeco2 _ = mempty

boringWiring :: (Sum Int, Z)
boringWiring =
  let acc = acc1 <> acc2
      (acc1, a) = makeA
      b = makeB
      c = makeC
      d = makeD
      e = makeE a
      f = makeF b c
      gDeco1 = makeGDeco1 a
      g = appEndo gDeco1 do makeG e f g
      zDeco1 = makeZDeco1 b e
      zDeco2 = makeZDeco2 f
      (acc2, h) = makeH a d g
      z = appEndo (zDeco2 <> zDeco1) do makeZ acc d h
   in (acc,z)

-- | Here we don't have to worry about positional parameters. We simply throw 
-- all the constructors into the 'Cauldron' and get the 'Z' value at the end,
-- plus a graph we may want to draw.
coolWiring :: Either Mishap (BeanGraph, Maybe (Sum Int, Z))
coolWiring =
  let cauldron =
        foldr
          ($)
          Cauldron.empty
          [ Cauldron.insert @A 
              do SimpleBean do Constructor do makeA & argsN <&> \(reg,a) -> regs1 reg a,
            Cauldron.insert @B 
              do Bean {
              constructor = Constructor do makeB & argsN <&> regs0,
              decos = mempty
            },
            Cauldron.insert @C 
              do Bean {
              constructor = Constructor do makeC & argsN <&> regs0,
              decos = mempty
            },
            Cauldron.insert @D 
              do Bean {
              constructor = Constructor do makeD & argsN <&> regs0,
              decos = mempty
            },
            Cauldron.insert @E 
             do Bean {
              constructor = Constructor do makeE & argsN <&> regs0,
              decos = mempty
            },
            Cauldron.insert @F 
            do Bean {
              constructor = Constructor do makeF & argsN <&> regs0,
              decos = mempty
            },
            Cauldron.insert @G 
            do Bean {
              constructor = Constructor do makeG & argsN <&> regs0,
              decos = Cauldron.decosFromList [
                Constructor do makeGDeco1 & argsN <&> regs0
              ]
            },
            Cauldron.insert @H 
            do Bean {
              constructor = Constructor do makeH & argsN <&> \(reg,a ) -> regs1 reg a,
              decos = mempty
            },
            Cauldron.insert @Z 
            do Bean {
              constructor = Constructor do makeZ & argsN <&> regs0,
              decos = Cauldron.decosFromList [
                Constructor do makeZDeco1 & argsN <&> regs0,
                Constructor do makeZDeco2 & argsN <&> regs0
              ]
            }
          ]
     in case Cauldron.cook cauldron of 
          Left e -> Left e
          Right (beanGraph, beans) ->
            Right (beanGraph, do
                     reg <- Cauldron.taste (Proxy @(Sum Int)) beans
                     z <- Cauldron.taste (Proxy @Z) beans
                     pure (reg,z))
  

main :: IO ()
main = do
  print boringWiring
  case coolWiring of
    Left mishap -> do
      print mishap
    Right (beanGraph, bean) -> do
      print bean
      Cauldron.exportToDot "beans.dot" beanGraph
