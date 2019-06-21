module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)

import System.Process (callCommand)


main :: IO ()
main = return ()

goldenTests :: IO TestTree
goldenTests = do
  files <- loadfiles
  return $ testGroup "TTC Golden Tests"
    [ goldenVsString
        "testName"
        "filepath"
        compilerCommand
      | file <- files
    ]