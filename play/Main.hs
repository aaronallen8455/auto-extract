{-# LANGUAGE CPP #-}
module Main where

import           Foo
-- com

-- com
main :: IO ()
main = pure ()

-- com
foo = let ttt = 12 in EXTRACT@hello (ttt + 19)

bar = 13
