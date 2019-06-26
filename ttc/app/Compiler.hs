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

data Compiler =
  Compiler
    { cInputs :: [String]
    , cOutput :: String
    , cOutputIR :: Bool
    }
    deriving (Show)

data SrcFile = SrcFile FilePath String

runCompiler :: Compiler -> IO ()
runCompiler c =
  mapM_ compileSTLC (cInputs c) 

lexSTLC :: FilePath -> String -> [STLC.Token]
lexSTLC fp c =
  case runExcept $ STLC.lex fp (pack c) of
    Left err -> do
      error $ show (pretty err)

    Right toks -> STLC.layout toks

compileSTLC :: String -> IO ()
compileSTLC fp = do
  toks <- (lexSTLC fp) <$> readFile fp
  putDoc $ pretty toks
  let stlc = STLC.parseModule toks

  let stlc' = STLC.checkModule stlc
  withFile (dropExtension fp ++ "-typed.stlc") WriteMode $ \h -> 
    hPutDoc h $ pretty stlc'
  
  --let stlc'' = matchModule stlc'
  --withFile (dropExtension fp ++ "-matched.stlc") WriteMode $ \h -> 
  --  hPutDoc h $ pretty stlc''

  let lltc = STLC.desugarModule stlc'
  withFile (replaceExtension fp "lltt") WriteMode $ \h -> 
    hPutDoc h $ pretty lltc

  let llvmir = LL.genModule LL.envEmpty lltc
  LLVM.withContext $ \c -> LLVM.withModuleFromAST c llvmir (LLVM.writeLLVMAssemblyToFile (LLVM.File (fp ++ ".ll")))
