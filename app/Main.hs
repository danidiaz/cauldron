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

{- 
  HERE ARE A BUNCH OF DATATYPES.

  The idea is that each datatype represents a bean, a component of our application.

  In a real application, these datatypes would be records containing effectful functions.

-}

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

{- 
  HERE ARE A BUNCH OF CONSTRUCTORS AND DECORATORS.

-}
makeA :: A
makeA = A

-- A bean with a monoidal registration.
-- 
-- The registration could be some generic introspection mechanism, or perhaps
-- some effectful action that sets up a worker thread.
makeB :: (Sum Int, B)
makeB = (Sum 1, B)

makeC :: C
makeC = C

makeD :: D
makeD = D

-- | A bean with an effectful initialization.
--
-- We might want this in order to allocate some internal IORef,
-- or perhaps to read some configuration file.
makeE :: IO (A -> E)
makeE = pure \_ -> E

-- | A bean with an effectful initialization and a monoidal registration.
makeF :: IO (B -> C -> (Sum Int, F))
makeF = pure \_ _ -> (Sum 1,F)

-- | A bean with a self-dependency! 
--
-- We need this if we want self-invocations to be decorated.
--
-- Dependency cycles of more than one bean are forbidden, however.
makeG :: E -> F -> G -> G 
makeG _ _ _ = G

-- | A decorator.
makeGDeco1 :: A -> Endo G
makeGDeco1 _ = mempty

makeH :: A -> D -> G -> (Sum Int, H)
makeH _ _ _ = (Sum 1, H)

makeZ :: Sum Int -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Endo Z
makeZDeco1 _ _ = mempty

-- | A decorator with effectful initialization and a monoidal registration.
makeZDeco2 :: IO (F -> (Sum Int, Endo Z))
makeZDeco2 = pure \_ -> (Sum 1, mempty)

boringWiring :: IO (Sum Int, Z)
boringWiring = do
  -- We need to run the effectful constructors first.
  makeE' <- makeE
  makeF' <- makeF
  makeZDeco2' <- makeZDeco2
  let 
      -- We have to remember to collect the monoidal registrations. 
      reg = reg1 <> reg2 <> reg3 <> reg4
      -- Now let's tie the constructors together.
      a = makeA
      (reg1, b) = makeB
      c = makeC
      d = makeD
      e = makeE' a
      (reg2, f) = makeF' b c
      gDeco1 = makeGDeco1 a
      -- Here we apply a single decorator.
      g = appEndo gDeco1 do makeG e f g
      zDeco1 = makeZDeco1 b e
      (reg3, zDeco2) = makeZDeco2' f
      (reg4, h) = makeH a d g
      -- Compose the decorators before applying them.
      z = appEndo (zDeco2 <> zDeco1) do makeZ reg d h
  pure (reg, z)

-- | Here we don't have to worry about positional parameters. We simply throw
-- all the constructors into the 'Cauldron' and taste the bean values at the
-- end, plus a graph we may want to draw.
--
-- Note that we detect wiring errors before running the initialization.
coolWiring :: Either BadBeans (BeanGraph, IO (Maybe (Sum Int, Z)))
coolWiring =
  let cauldron :: Cauldron IO =
        foldr
          ($)
          mempty
          [ insert @A do bare do pack_ do pure makeA,
            insert @B do bare do pack (\(reg, bean) -> regs1 reg bean) do pure makeB,
            insert @C do bare do pack_ do pure makeC,
            insert @D do bare do pack_ do pure makeD,
            insert @E do bare do pack_ do makeE,
            insert @F do bare do pack (\(reg, bean) -> regs1 reg bean) do makeF,
            insert @G do
              Bean
                { constructor = pack_ do pure makeG,
                  decos =
                    fromConstructors
                      [ pack_ do pure makeGDeco1
                      ]
                },
            insert @H do bare do pack (\(reg, bean) -> regs1 reg bean) do pure makeH,
            insert @Z do
              Bean
                { constructor = pack_ do pure makeZ,
                  decos =
                    fromConstructors
                      [ pack_ do pure makeZDeco1,
                        pack (\(reg, bean) -> regs1 reg bean) do makeZDeco2
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
  do wiring <- boringWiring
     print wiring
  case coolWiring of
    Left mishap -> do
      print mishap
    Right (beanGraph, action) -> do
      result <- action
      print result
      exportToDot "beans.dot" beanGraph
