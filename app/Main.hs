module Main where

import qualified MagicCauldron (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MagicCauldron.someFunc
