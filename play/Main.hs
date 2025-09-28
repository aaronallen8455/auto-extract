module Main where

import           Foo
-- com

-- com
main :: IO ()
main = pure ()

z = 20

-- com
x :: Int
x = let y = 12
     in EXTRACT@hello (y + 19 * z)
