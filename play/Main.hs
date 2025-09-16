{-# LANGUAGE CPP #-}
module Main where

-- com
main :: IO ()
main = pure ()

foo = let ttt = 12 in EXTRACT@hello (ttt + 19)

bar = 13
