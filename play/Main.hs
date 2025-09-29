module Main where

import           Foo
-- com

-- com
main :: IO ()
main = pure ()

doubleInput :: IO ()
doubleInput = do
  input <- EXTRACT@promptNumber (do
    putStrLn "Enter a number"
    readLn)
  let doubled = EXTRACT@double (input * 2)
  print doubled
