{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified System.Directory as Dir
import qualified System.Process as Proc

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "case"
    [ testCase "1" $ runTest "Case1.hs"
    , testCase "2" $ runTest "Case2.hs"
    , testCase "3" $ runTest "Case3.hs"
    , testCase "4" $ runTest "Case4.hs"
    , testCase "5" $ runTest "Case5.hs"
    , testCase "6" $ runTest "Case6.hs"
    , testCase "7" $ runTest "Case7.hs"
    , testCase "8" $ runTest "Case8.hs"
    , testCase "9" $ runTest "Case9.hs"
#if MIN_VERSION_ghc(9,12,0) || !MIN_VERSION_ghc(9,10,0)
    , testCase "10a" $ runTest "Case10a.hs"
#else
    , testCase "10b" $ runTest "Case10b.hs" -- 9.10 indentation issue
#endif
    , testCase "11" $ runTest "Case11.hs"
    , testCase "12" $ runTest "Case12.hs"
    ]
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

-- copy the input file contents to the module file to be compiled
prepTest :: FilePath -> IO ()
prepTest modFile = do
  inp <- readFile (modFile ++ ".input")
  writeFile modFile inp

runTest :: FilePath -> Assertion
runTest name = do
  let modFile = testModulePath name
  prepTest modFile
  (_, _, _, h) <- Proc.createProcess $
    Proc.proc "cabal" ["build", "test-modules:" ++ takeWhile (/= '.') name]
  void $ Proc.waitForProcess h
  updatedMod <- readFile modFile
  expectedMod <- readFile $ modFile ++ ".expected"
  assertEqual "Expected update" expectedMod updatedMod
  Dir.removeFile modFile
