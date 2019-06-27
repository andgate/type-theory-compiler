{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List
import Data.Maybe (fromMaybe)

import qualified Data.Text.IO as T

import Compiler

import Language.STLC.Desugar
import Language.STLC.Match
import Language.STLC.Pretty
import Language.STLC.Syntax
import Language.STLC.TypeCheck

import Language.LLTT.Pretty
import Language.LLTT.LLVM.Codegen

import qualified LLVM.Module as LLVM
import qualified LLVM.Internal.Context as LLVM
import qualified LLVM.AST as AST

import Unbound.Generics.LocallyNameless


import Data.Text.Prettyprint.Doc (Pretty (..))
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)

import System.IO
import Options.Applicative
{-

mallocExtern = ExternDefn $ Extern "malloc" [TI32] (TPtr TI8)
freeExtern = ExternDefn $ Extern "free" [TPtr TI8] (TVoid)
memcpyExtern = ExternDefn $ Extern "memcpy" [TPtr TI8, TPtr TI8, TI32] (TPtr TI8)
putsExtern = ExternDefn $ Extern "puts" [TString] TI32


derefIntFunc = FuncDefn $ func "derefInt"
                               (tarr [TPtr TI32] TI32)
                               [(PType (pvar "a") (TPtr TI32))]
                               body
  where body = EDeref (evar "a")

maybeIntType = DataTypeDefn $ DataType "MaybeInt" [("Nothing", []), ("Just", [(Nothing, TI32)])]

iVector3Type = DataTypeDefn $ DataType "IVector3" [("V3", [(Just "x", TI32), (Just "y", TI32), (Just "z", TI32)])]

nothingFunc = FuncDefn $ func "nothing" (TCon "MaybeInt") [] body 
  where body = ECon "Nothing" []

just5Func = FuncDefn $ func "just5" (TPtr $ TCon "MaybeInt") [] body
  where body = ENewCon "Just" [ELit $ LInt 5]


dotFunc = FuncDefn $ func "dot"
                          ( tarr [TCon "IVector3", TCon "IVector3"] TI32 )
                          [ PType (pvar "v1") (TCon "IVector3")
                          , PType (pvar "v2") (TCon "IVector3") ]
                          body
  where body  = elet [ (pvar "x1", EGet (evar "v1") "x")
                     , (pvar "y1", EGet (evar "v1") "y")
                     , (pvar "z1", EGet (evar "v1") "z")
                     , (pvar "x2", EGet (evar "v2") "x")
                     , (pvar "y2", EGet (evar "v2") "y")
                     , (pvar "z2", EGet (evar "v2") "z")
                     , (pvar "a1", EOp $ OpMulI (evar "x1") (evar "x2"))
                     , (pvar "a2", EOp $ OpMulI (evar "y1") (evar "y2"))
                     , (pvar "a3", EOp $ OpMulI (evar "z1") (evar "z2"))
                     , (pvar "a4", EOp $ OpAddI (evar "a1") (evar "a2"))
                     , (pvar "a5", EOp $ OpAddI (evar "a3") (evar "a4")) ]
                     $ evar "a5"


exMaybeFunc = FuncDefn $ func "exMaybe"
                              (tarr [TCon "MaybeInt"] TI32)
                              [PType (pvar "may_x") (TCon "MaybeInt")]
                              body
  where body = ecase (evar "may_x")
                     [ (PCon "Just" [pvar "x"], evar "x")
                     , (PCon "Nothing"  [], ELit $ LInt 0)
                     ]

addFunc = FuncDefn $ func "add"
                          (tarr [TI32, TI32] TI32)
                          [ PType (pvar "a") TI32
                          , PType (pvar "b") TI32]
                          body 
  where body = EOp $ OpAddI (evar "a") (evar "b")

mulFunc = FuncDefn $ func "mul"
                          (tarr [TI32, TI32] TI32)
                          [ PType (pvar "a") TI32
                          , PType (pvar "b") TI32]
                          body 
  where body = EOp $ OpMulI (evar "a") (evar "b")

constFunc = FuncDefn $ func "const"
                          (tarr [TI32, TI32] TI32)
                          [ PType (pvar "a") TI32
                          , PType (pvar "b") TI32]
                          body 
  where body = evar "a"

idFunc = FuncDefn $ func "id"
                          (tarr [TI32] TI32)
                          [ PType (pvar "x") TI32 ]
                          body 
  where body = evar "x"


idMaybeFunc = FuncDefn $ func "idMaybe"
                          (tarr [TCon "MaybeInt"] (TCon "MaybeInt"))
                          [ PType (pvar "x") (TCon "MaybeInt") ]
                          body 
  where body = evar "x"


addMulFunc = FuncDefn $ func "addMul"
                        (tarr [TI32, TI32, TI32] TI32)
                        [ PType (pvar "a") TI32
                        , PType (pvar "b") TI32
                        , PType (pvar "c") TI32]
                        body
  where body = elet [ (pvar "d", eapp "add" [evar "a", evar "b"])
                    , (pvar "e", eapp "mul" [evar "d", evar "c"]) ] 
                    $ eapp "id" [evar "e"]


maybeAddMulFunc = FuncDefn $ func "maybeAddMul"
                                  ( tarr [ TCon "MaybeInt"
                                         , TCon "MaybeInt"
                                         , TCon "MaybeInt" ] 
                                         (TCon "MaybeInt") )
                                  [ PType (pvar "may_a") (TCon "MaybeInt")
                                  , PType (pvar "may_b") (TCon "MaybeInt")
                                  , PType (pvar "may_c") (TCon "MaybeInt") ]
                                  body
  where body = ecase (evar "may_a")
                      [ ( PCon "Nothing" [], ECon "Nothing" [])
                      , ( PCon "Just" [pvar "a"], case2) ]
        case2 = ecase (evar "may_b")
                      [ (PCon "Nothing" [] , ECon "Nothing" [])
                      , (PCon "Just" [pvar "b"], case3) ]
        case3 = ecase (evar "may_c")
                      [ (PCon "Nothing" [], ECon "Nothing" [])
                      , (PCon "Just" [pvar "c"], let1) ]
        let1 = elet [ (pvar "d", eapp "addMul" [evar "a", evar "b", evar "c"]) ]
                    (ECon "Just" [evar "d"])

mainFunc = FuncDefn $ func "main"
                           (tarr [TI32, TPtr (TPtr TI8)] TI32)
                           [ pvar "argc", pvar "argv" ]
                           body 
  where body = elet [ (pvar "hello", ELit $ LString "Hello World")
                    , (pvar "five", ELit $ LString "5")
                    , (pvar "may_5_ptr", evar "just5")
                    , (pvar "may_5", EDeref $ evar "may_5_ptr")
                    , (pvar "may_not", evar "nothing") ]
                    case1
        case1 = ecase (evar "may_5")
                      [ (PCon "Nothing" [], eapp "puts" [evar "hello"])
                      , (PCon "Just" [pvar "c"], eapp "puts" [evar "five"])       
                      ]

testSource :: Module
testSource 
  = Module  "Test"
            [ mallocExtern
            , freeExtern
            , memcpyExtern
            , putsExtern
            , derefIntFunc
            , maybeIntType
            , iVector3Type
            , nothingFunc
            , just5Func
            , dotFunc
            , exMaybeFunc
            , addFunc
            , mulFunc
            , constFunc
            , idFunc
            , idMaybeFunc
            , addMulFunc
            , maybeAddMulFunc
            , mainFunc
            ]


compileModule :: FilePath -> Module -> IO ()
compileModule fp modl = do
  let stlc = modl
  withFile (fp ++ ".stlc") WriteMode $ \h -> 
    hPutDoc h $ pretty stlc

  let stlc = modl
  withFile (fp ++ ".stlc") WriteMode $ \h -> 
    hPutDoc h $ pretty stlc

  let stlc' = checkModule stlc
  withFile (fp ++ "-typed.stlc") WriteMode $ \h -> 
    hPutDoc h $ pretty stlc'
  
  --let stlc'' = matchModule stlc'
  --withFile (fp ++ "-matched.stlc") WriteMode $ \h -> 
  --  hPutDoc h $ pretty stlc''

  let lltc = desugarModule stlc'
  withFile (fp ++ ".lltt") WriteMode $ \h -> 
    hPutDoc h $ pretty lltc

  let llvmir = genModule envEmpty lltc
  LLVM.withContext $ \c -> LLVM.withModuleFromAST c llvmir (LLVM.writeLLVMAssemblyToFile (LLVM.File (fp ++ ".ll")))
-}


data Options
  = Options { optInputs :: [String]
            , optOutput :: String
            , optOutputIR :: Bool
            , optBuildDir :: FilePath
            }
  deriving (Eq, Show)

options :: Parser Options
options = do
  optOutputIR <- switch
                  ( long "output-ir"
                  <> help "Output all ir representations" )
  optOutput <- strOption
                  ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> value "a.out"
                  <> help "Write output to FILE" )
  optBuildDir <- strOption
                  ( long "build-dir"
                  <> metavar "FILE"
                  <> value "./"
                  <> help "Write build output to FILE" )
  optInputs <- many (argument str idm)
  pure Options {..}

opt2c :: Options -> Compiler
opt2c Options {..}
  = mkCompiler
      { cInputs = optInputs
      , cOutput = optOutput
      , cOutputIR = optOutputIR
      , cBuildDir = optBuildDir
      }

opts :: ParserInfo Options
opts = info (options <**> helper) idm

main :: IO ()
main = execParser opts >>= runCompiler . opt2c