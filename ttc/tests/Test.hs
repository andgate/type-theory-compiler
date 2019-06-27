module Main where

import Control.Monad
import Data.List

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeBaseName, replaceExtension, (</>))
import System.Process (callCommand, waitForProcess)

import System.Exit (exitWith, ExitCode(..), die)


main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  singleFileTests <- goldenTestSingle
  multiFileTests <- mempty -- goldenTestMulti
  return $ testGroup "TTC Golden Tests" (singleFileTests <> multiFileTests)

goldenTestSingle :: IO [TestTree]
goldenTestSingle = do
  let testsDir = "tests/single"
      outDir = "tests/out/single"
      binDir = "tests/bin/single"
  srcPaths <- findByExtension [".stlc"] testsDir
  createDirectoryIfMissing True outDir
  createDirectoryIfMissing True binDir

  return $
    [ goldenVsFile
        ("Golden Test " ++ takeBaseName srcPath ++ " (Single-File)")
        goldPath
        outPath
        (compileFile srcPath outPath)
    | srcPath <- srcPaths
    , let outPath = outDir </> takeBaseName srcPath <> ".out"
          goldPath = replaceExtension srcPath "gold"
    ]


goldenTestMulti :: IO [TestTree]
goldenTestMulti = do
  let testsDir = "tests/multi"
      outDir = "tests/out/multi"
      binDir = "tests/bin/multi"
  srcDirs <- filterM doesDirectoryExist . map (testsDir </>) =<< listDirectory testsDir
  createDirectoryIfMissing True outDir
  createDirectoryIfMissing True binDir
  return $
    [ goldenVsFile
        ("Golden Test " ++ takeBaseName srcDir ++ " (Multi-File)")
        goldPath
        outPath
        (compileDir srcDir outPath)
    | srcDir <- srcDirs
    , let outPath = outDir </> takeBaseName srcDir <> ".out"
          goldPath = replaceExtension srcDir ".gold"
    ]


compileFile :: FilePath -> FilePath -> IO ()
compileFile srcPath outPath = do
  let exePath = "tests/bin/single/" <> takeBaseName srcPath
  let buildPath = "tests/build/single/" <> takeBaseName srcPath <> "/"
  callCommand $ "cabal new-run ttc -- " 
             ++ srcPath ++ " -o " ++ exePath
             ++ " --build-dir " ++ buildPath
  callCommand $ exePath ++ " > " ++ outPath

compileDir :: FilePath -> FilePath -> IO ()
compileDir srcDir outPath = do
  let exePath = "tests/bin/multi/" <> takeBaseName srcDir
  srcPaths <- findByExtension [".stlc"] srcDir
  callCommand $ "cabal new-run ttc -- " ++ intercalate " " srcPaths ++ " -o " ++ exePath
  callCommand $ exePath ++ " > " ++ outPath