{-# LANGUAGE BlockArguments #-}
-- | We have a bunch of datatypes, and a single recipe (constructor) for each
-- datatype. This means the wiring can be type-directed: we don't have to make a
-- decision at the term level about which constructor to use. 
--
-- We wire the constructors in two ways: manually, and using a bit of dynamic
-- typing magic from the "Cauldron" module.
module Main where

import Cauldron (BeanGraph, Mishap, argsN, regs0)
import Cauldron qualified
import Data.Proxy
import Data.Function ((&))
import Data.Functor ((<&>))

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

makeA :: A
makeA = A

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

makeG :: E -> F -> G
makeG _ _ = G

makeH :: A -> D -> G -> H
makeH _ _ _ = H

makeZ :: D -> H -> Z
makeZ _ _ = Z

boringWiring :: Z
boringWiring =
  let a = makeA
      b = makeB
      c = makeC
      d = makeD
      e = makeE a
      f = makeF b c
      g = makeG e f
      h = makeH a d g
   in makeZ d h

-- | Here we don't have to worry about positional parameters. We simply throw 
-- all the constructors into the 'Cauldron' and get the 'Z' value at the end,
-- plus a graph we may want to draw.
coolWiring :: Either Mishap (BeanGraph, Maybe Z)
coolWiring =
  let cauldron =
        foldr
          ($)
          Cauldron.empty
          [ Cauldron.insert do argsN makeA <&> regs0,
            Cauldron.insert do argsN makeB <&> regs0,
            Cauldron.insert do argsN makeC <&> regs0,
            Cauldron.insert do argsN makeD <&> regs0,
            Cauldron.insert do argsN makeE <&> regs0,
            Cauldron.insert do argsN makeF <&> regs0,
            Cauldron.insert do argsN makeG <&> regs0,
            Cauldron.insert do argsN makeH <&> regs0,
            Cauldron.insert do argsN makeZ <&> regs0
          ]
     in case Cauldron.boil cauldron of 
          Left e -> Left e
          Right (beanGraph, beans) ->
            Right (beanGraph, Cauldron.taste (Proxy @Z) beans)
  

main :: IO ()
main = do
  print boringWiring
  case coolWiring of
    Left mishap -> do
      print mishap
    Right (beanGraph, bean) -> do
      print bean
      Cauldron.exportToDot "beans.dot" beanGraph
