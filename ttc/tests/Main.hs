module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeBaseName, replaceExtension, (</>))
import System.Process (spawnCommand, waitForProcess)

import System.Exit (exitWith, ExitCode(..), die)


main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  singleFileTests <- goldenTestSingle
  multiFileTests <- goldenTestMulti
  return $ testGroup "Hawk Golden Tests" (singleFileTests <> multiFileTests)

goldenTestSingle :: IO [TestTree]
goldenTestSingle = do
  let testsDir = "tests/single"
      outDir = "tests/out/single"
      binDir = "tests/bin/single"
  srcPaths <- findByExtension [".hk"] testsDir
  createDirectoryIfMissing True outDir
  createDirectoryIfMissing True binDir

  return $
    [ goldenVsFile
        ("Golden Test " ++ takeBaseName srcPath ++ " (Single-File)")
        goldPath
        outPath
        (compileFile srcPath outPath)
    | srcPath <- srcPaths
    , let outPath = outDir <> takeBaseName srcPath <> ".out"
          goldPath = replaceExtension srcPath ".gold"
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
  let exePath = "tests/bin/single/" <> takeBaseName srcPath <> ".exe"
  h1 <- spawnCommand $ "cabal new-run hcc -- " ++ srcPath ++ " -o " ++ exePath
  exit_code <- waitForProcess h1

  case exit_code of
    ExitSuccess -> return ()
    ExitFailure _ -> exitWith exit_code


  h2 <- spawnCommand $ exePath ++ " >> " ++ outPath
  exitWith =<< waitForProcess h2

compileDir :: FilePath -> FilePath -> IO ()
compileDir srcDir outPath = do
  let exePath = "tests/bin/multi/" <> takeBaseName srcDir <> ".exe"
  srcPaths <- findByExtension [".hk"] srcDir
  h1 <- spawnCommand $ "cabal new-run ttc -- " ++ intercalate " " srcPaths ++ " -o " ++ exePath
  exit_code <- waitForProcess h1

  case exit_code of
    ExitSuccess -> return ()
    ExitFailure _ -> exitWith exit_code


  h2 <- spawnCommand $ exePath ++ " >> " ++ outPath
  exitWith =<< waitForProcess h2