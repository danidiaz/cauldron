{-# LANGUAGE BlockArguments #-}

-- | We have a bunch of datatypes, and a single recipe (constructor) for each
-- datatype. This means the wiring can be type-directed: we don't have to make a
-- decision at the term level about which constructor to use.
--
-- We wire the constructors in two ways: manually, and using a bit of dynamic
-- typing magic from the "Cauldron" module.
module Main where

import Cauldron
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
   in (acc, z)

-- | Here we don't have to worry about positional parameters. We simply throw
-- all the constructors into the 'Cauldron' and get the 'Z' value at the end,
-- plus a graph we may want to draw.
coolWiring :: Either Mishap (BeanGraph, IO (Maybe (Sum Int, Z)))
coolWiring =
  let cauldron :: Cauldron IO =
        foldr
          ($)
          mempty
          [ insert @A do bare do pack (\(reg, bean) -> regs1 reg bean) do pure makeA,
            insert @B do bare do pack_ do pure makeB,
            insert @C do bare do pack_ do pure makeC,
            insert @D do bare do pack_ do pure makeD,
            insert @E do bare do pack_ do pure makeE,
            insert @F do bare do pack_ do pure makeF,
            insert @G do
              Bean
                { constructor = pack_ do pure makeG,
                  decos =
                    fromConstructors
                      [ pack_ do pure makeGDeco1
                      ]
                },
            insert @H do
              Bean
                { constructor = pack (\(reg, bean) -> regs1 reg bean) do pure makeH,
                  decos = mempty
                },
            insert @Z do
              Bean
                { constructor = pack_ do pure makeZ,
                  decos =
                    fromConstructors
                      [ pack_ do pure makeZDeco1,
                        pack_ do pure makeZDeco2
                      ]
                }
          ]
   in case cook cauldron of
        Left e -> Left e
        Right (beanGraph, action) ->
          Right (beanGraph, do
            beans <- action
            pure do 
              reg <- taste @(Sum Int) beans
              z <- taste @Z beans
              pure (reg, z)
            )

main :: IO ()
main = do
  print boringWiring
  case coolWiring of
    Left mishap -> do
      print mishap
    Right (beanGraph, action) -> do
      result <- action
      print result
      exportToDot "beans.dot" beanGraph
