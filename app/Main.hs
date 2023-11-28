{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

-- | We have a bunch of datatypes, and a single recipe (constructor) for each
-- datatype. This means the wiring can be type-directed: we don't have to make a
-- decision at the term level about which constructor to use.
--
-- We wire the constructors in two ways: manually, and using a bit of dynamic
-- typing magic from the "Cauldron" module.
module Main where

import Cauldron
import Data.Monoid
import Data.Function ((&))

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
  These beans are a bit special: they are "secondary" beans which are optionally
  produced by the constructors of other beans.

  They have Monoid instances. The values returned by all the constructors that
  produce them will be combined.
-}

-- | A window into the internal state of some bean.
newtype Inspector = Inspector {inspect :: IO [String]}
  deriving newtype (Semigroup, Monoid)

-- | And initialization action which some beans might register.
newtype Initializer = Initializer {runInitializer :: IO ()}
  deriving newtype (Semigroup, Monoid)

{-
  HERE ARE A BUNCH OF CONSTRUCTORS AND DECORATORS.

-}
makeA :: A
makeA = A

-- A bean with a monoidal registration.
--
-- The registration could be some generic introspection mechanism, or perhaps
-- some effectful action that sets up a worker thread.
makeB :: (Inspector, B)
makeB = (Inspector (pure ["B stuff"]), B)

makeC :: C
makeC = C

makeD :: D
makeD = D

-- | A bean with an effectful constructor.
--
-- We might want this in order to allocate some internal IORef,
-- or perhaps to read some configuration file.
makeE :: IO (A -> E)
makeE = pure \_ -> E

-- | A bean with an effectful constructor and a monoidal registration.
makeF :: IO (B -> C -> (Inspector, F))
makeF = pure \_ _ -> (Inspector (pure ["F stuff"]), F)

makeG :: E -> F -> G
makeG _ _ = G

-- | A decorator.
--
--  Decorators are basically normal constructors, only that they return
--  a Endo that knows how to tweak the value of a bean.
--
-- Because they are normal constructors, they can be effectful, and they
-- might have dependencies of their own.
makeGDeco1 :: A -> G ->  G
makeGDeco1 _ g = g

-- | A bean with two monoidal registrations.
makeH :: A -> D -> G -> (Initializer, Inspector, H)
makeH _ _ _ = (Initializer (putStrLn "H init"), Inspector (pure ["H stuff"]), H)

-- | Notice that this bean has "Inspector" as a dependency. Inspector is a
-- monoidal bean which is aggregated across all the constructor that register
-- it. This is OK as long as there are no dependency cycles.
--
-- Why would a bean depend on such a aggregated bean? Well, for example, a
-- server bean might want to publish diagnostic information collected from beans
-- that register it.
makeZ :: Inspector -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Z-> Z
makeZDeco1 _ _ z = z

-- | A decorator with an effectful constructor and a monoidal registration.
makeZDeco2 :: IO (F -> Z -> (Initializer, Z))
makeZDeco2 = pure \_ z -> (Initializer (putStrLn "Z deco init"), z)

boringWiring :: IO (Initializer, Inspector, Z)
boringWiring = do
  -- We need to run the effectful constructors first.
  makeE' <- makeE
  makeF' <- makeF
  makeZDeco2' <- makeZDeco2
  let -- We have to remember to collect the monoidal registrations.
      initializer = init1 <> init2
      -- We have to remember to collect the monoidal registrations.
      inspector = inspector1 <> inspector2 <> inspector3
      -- Now let's tie the constructors together.
      a = makeA
      (inspector1, b) = makeB
      c = makeC
      d = makeD
      e = makeE' a
      (inspector2, f) = makeF' b c
      g0 = makeG e f
      g1 = makeGDeco1 a g0
      g = g1
      -- Here we apply a single decorator.
      (init1, inspector3, h) = makeH a d g
      -- Compose the decorators before applying them.
      z0 = makeZ inspector d h
      z1 = makeZDeco1 b e z0
      (init2, z2) = makeZDeco2' f z1
      z = z2
  pure (initializer, inspector, z)

-- | Here we don't have to worry about positional parameters. We throw all the
-- constructors into the 'Cauldron' and taste the bean values at the end, plus a
-- graph we may want to draw.
--
-- Note that we detect wiring errors *before* running the effectful constructors.
coolWiring :: Either BadBeans (DependencyGraph, IO (Maybe (Initializer, Inspector, Z)))
coolWiring =
  let cauldron :: Cauldron IO IO =
        empty
          & insert @A do bare do pack (pure . regs0) do pure makeA
          & insert @B do bare do pack (pure . (\(reg, bean) -> regs1 reg bean)) do pure makeB
          & insert @C do bare do pack (pure . regs0) do pure makeC
          & insert @D do bare do pack (pure . regs0) do pure makeD
          & insert @E do bare do pack (pure . regs0) do makeE
          & insert @F do bare do pack (pure . (\(reg, bean) -> regs1 reg bean)) do makeF
          & insert @G do
            Bean
              { constructor = pack (pure . regs0) do pure makeG,
                decos =
                  fromConstructors
                    [ pack (pure . regs0) do pure makeGDeco1
                    ]
              }
          & insert @H do bare do pack (pure . (\(reg1, reg2, bean) -> regs2 reg1 reg2 bean)) do pure makeH
          & insert @Z do
            Bean
              { constructor = pack (pure .  regs0) do pure makeZ,
                decos =
                  fromConstructors
                    [ pack (pure . regs0) do pure makeZDeco1,
                      pack (pure . (\(reg, bean) -> regs1 reg bean)) do makeZDeco2
                    ]
              }
   in case cook cauldron of
        Left e -> Left e
        Right (depGraph, action) ->
          Right
            ( depGraph,
              do
                innerAction <- action
                beans <- innerAction
                pure do
                  initializer <- taste @Initializer beans
                  inspector <- taste @Inspector beans
                  z <- taste @Z beans
                  pure (initializer, inspector, z)
            )

main :: IO ()
main = do
  do
    (Initializer {runInitializer}, Inspector {inspect}, z) <- boringWiring
    inspection <- inspect
    print inspection
    print z
    runInitializer
  case coolWiring of
    Left badBeans -> do
      print badBeans
    Right (depGraph, action) -> do
      exportToDot "beans.dot" depGraph
      result <- action
      case result of
        Nothing -> print "oops"
        Just (Initializer {runInitializer}, Inspector {inspect}, z) -> do
          inspection <- inspect
          print inspection
          print z
          runInitializer
