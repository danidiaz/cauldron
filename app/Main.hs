module Main where

import Cauldron (Cauldron)

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show
data D = D deriving Show
data E = E deriving Show
data F = F deriving Show
data G = G deriving Show
data H = H deriving Show
data I = I deriving Show
data J = J deriving Show
data K = K deriving Show
data L = L deriving Show
data M = M deriving Show
data N = N deriving Show
data O = O deriving Show
data P = P deriving Show
data Q = Q deriving Show
data R = R deriving Show
data S = S deriving Show
data T = T deriving Show
data U = U deriving Show
data V = V deriving Show
data W = W deriving Show
data X = X deriving Show
data Y = Y deriving Show
data Z = Z deriving Show

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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
