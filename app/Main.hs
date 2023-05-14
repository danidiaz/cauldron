module Main where

import qualified MagicCauldron (someFunc)

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

a :: A
a = A

b :: B
b = B

c :: C
c = C

d :: D 
d = D

c1 :: A -> E
c1 _ = E

c2 :: B -> F
c2 _ = F

c3 :: E -> F -> G
c3 _ _ = G

c4 :: A -> D -> G -> H
c4 _ _ _ = H

c5 :: D -> H -> Z
c5 _ _ = Z

boringWiring :: Z
boringWiring =
  c5 d (c4 a d (c3 (c1 a) (c2 b)))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MagicCauldron.someFunc
