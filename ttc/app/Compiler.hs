{-# LANGUAGE RecordWildCards #-}
module Compiler where

import Control.Monad.Except

import qualified Language.STLC.Desugar as STLC
import qualified Language.STLC.Match as STLC
import Language.STLC.Pretty
import qualified Language.STLC.Lex as STLC
import qualified Language.STLC.Lex.Format as STLC
import qualified Language.STLC.Lex.Token as STLC
import qualified Language.STLC.Parse as STLC
import qualified Language.STLC.Syntax as STLC
import qualified Language.STLC.TypeCheck as STLC

import Language.LLTT.Pretty
import qualified Language.LLTT.LLVM.Codegen as LL

import qualified LLVM.Module as LLVM
import qualified LLVM.Internal.Context as LLVM
import qualified LLVM.AST as AST

import Unbound.Generics.LocallyNameless

import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc, hPutDoc)

import System.IO
import System.FilePath
import System.Directory
import System.Process (callCommand, waitForProcess)
import System.Exit (exitWith, ExitCode(..), die)


data Compiler =
  Compiler
    { cInputs :: [String]
    , cOutput :: String
    , cOutputIR :: Bool
    , cBuildDir :: FilePath
    }
    deriving (Show)

data SrcFile = SrcFile FilePath String

mkCompiler :: Compiler
mkCompiler
  = Compiler
      { cInputs = ["main.stlc"]
      , cOutput = "a.out"
      , cOutputIR = False
      , cBuildDir = "./"
      }

runCompiler :: Compiler -> IO ()
runCompiler Compiler{..} = do
  createDirectoryIfMissing True cBuildDir
  createDirectoryIfMissing True (takeDirectory cOutput)
  llmodules <- mapM (compileSTLC cBuildDir) cInputs
  callCommand $ "clang-8 -O2 rts.c " <> unwords llmodules <> " -o " <> cOutput
 
lexSTLC :: FilePath -> String -> [STLC.Token]
lexSTLC fp c =
  case runExcept $ STLC.lex fp (pack c) of
    Left err -> do
      error $ show (pretty err)

    Right toks -> STLC.layout toks

compileSTLC :: String -> String -> IO String
compileSTLC build_dir in_fp = do
  toks <- (lexSTLC in_fp) <$> readFile in_fp
  -- putDoc $ pretty toks <> line
  let stlc = STLC.parseModule toks

  let build_fp = build_dir <> takeBaseName in_fp

  let stlc' = STLC.checkModule stlc
  withFile (build_fp <> ".stlc.typed") WriteMode $ \h ->
    hPutDoc h $ pretty stlc'
  
  --let stlc'' = matchModule stlc'
  --withFile (build_fp <> ".stlc.matched") WriteMode $ \h -> 
  --  hPutDoc h $ pretty stlc''

  let lltc = STLC.desugarModule stlc'
  withFile (build_fp <> ".lltt") WriteMode $ \h -> 
    hPutDoc h $ pretty lltc

  let llvmir = LL.genModule LL.envEmpty lltc
      irfp = build_fp <> ".ll"
  LLVM.withContext $ \c -> LLVM.withModuleFromAST c llvmir (LLVM.writeLLVMAssemblyToFile (LLVM.File irfp))

  callCommand $ "clang-8 -O2 -S -emit-llvm " <> irfp <> " -o " <> irfp <> ".opt"

  return irfp