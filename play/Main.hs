{-# LANGUAGE CPP #-}
module Main where

import           Foo
-- com

-- com
main :: IO ()
main = pure ()

-- com
x :: Int
x = let ttt = 12 in EXTRACT@hello (ttt + 19 + x)
