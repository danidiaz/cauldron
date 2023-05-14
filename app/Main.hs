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

a2e :: A -> E
a2e _ = E

b2f :: B -> F
b2f _ = F

ef2g :: E -> F -> G
ef2g _ _ = G

g2h :: G -> H
g2h _ = H

dh2z :: D -> H -> Z
dh2z _ _ = Z

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MagicCauldron.someFunc
