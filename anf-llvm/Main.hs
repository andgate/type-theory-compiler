{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.ANF.Syntax
import Language.ANF.LLVM.Codegen

import qualified LLVM.Module as LLVM
import qualified LLVM.Internal.Context as LLVM
import qualified LLVM.AST as AST

mallocExtern = ExternDefn $ Extern "malloc" [TI32] (TPtr TI8)
freeExtern = ExternDefn $ Extern "free" [TPtr TI8] (TVoid)
memcpyExtern = ExternDefn $ Extern "memcpy" [TPtr TI8, TPtr TI8, TI32] (TPtr TI8)
putsExtern = ExternDefn $ Extern "puts" [TPtr TI8] TI32


derefIntFunc = FuncDefn $ Func "derefInt" [("a", TPtr TI32)] body TI32
  where body = EOp $ PtrOp $ DerefOp TI32 (VVar "a")

maybeIntType = DataTypeDefn $ DataType "MaybeInt" [("Nothing", []), ("Just", [(Nothing, TI32)])]

iVector3Type = DataTypeDefn $ DataType "IVector3" [("V3", [(Just "x", TI32), (Just "y", TI32), (Just "z", TI32)])]

nothingFunc = FuncDefn $ Func "nothing" [] body (TCon "MaybeInt")
  where body = EOp $ MemOp $ ConstrOp (Constr "Nothing" [])

just5Func = FuncDefn $ Func "just5" [] body (TPtr $ TCon "MaybeInt")
  where body = EOp $ MemOp $ NewOp (Constr "Just" [VInt 5])


dotFunc = FuncDefn $ Func "dot" [("v1", TCon "IVector3"), ("v2", TCon "IVector3")] body TI32
  where body  = ELet (Just "x1") TI32 (EOp $ MemOp $ MemAccess (VVar "v1") "x") TI32
              $ ELet (Just "y1") TI32 (EOp $ MemOp $ MemAccess (VVar "v1") "y") TI32
              $ ELet (Just "z1") TI32 (EOp $ MemOp $ MemAccess (VVar "v1") "z") TI32
              $ ELet (Just "x2") TI32 (EOp $ MemOp $ MemAccess (VVar "v2") "x") TI32
              $ ELet (Just "y2") TI32 (EOp $ MemOp $ MemAccess (VVar "v2") "y") TI32
              $ ELet (Just "z2") TI32 (EOp $ MemOp $ MemAccess (VVar "v2") "z") TI32
              $ ELet (Just "a1") TI32 (EOp $ IArithOp $ MulOpI (VVar "x1") (VVar "x2")) TI32
              $ ELet (Just "a2") TI32 (EOp $ IArithOp $ MulOpI (VVar "y1") (VVar "y2")) TI32
              $ ELet (Just "a3") TI32 (EOp $ IArithOp $ MulOpI (VVar "z1") (VVar "z2")) TI32
              $ ELet (Just "a4") TI32 (EOp $ IArithOp $ AddOpI (VVar "a1") (VVar "a2")) TI32
              $ ELet (Just "a5") TI32 (EOp $ IArithOp $ AddOpI (VVar "a3") (VVar "a4")) TI32
              $ EVal (VVar "a5")


exMaybeFunc = FuncDefn $ Func "exMaybe" [("may_x", TCon "MaybeInt")] body TI32
  where body = EMatch (VVar "may_x") TI32
                      [ ( "Just"   , [Just "x"], EVal $ VVar "x")
                      , ( "Nothing" , [], EVal $ VInt 0) ]

addFunc = FuncDefn $ Func "add" [("a", TI32), ("b", TI32)] body TI32
  where body = EOp (IArithOp (AddOpI (VVar "a") (VVar "b")))

mulFunc = FuncDefn $ Func "mul" [("a", TI32), ("b", TI32)] body TI32
  where body = EOp (IArithOp (MulOpI (VVar "a") (VVar "b")))

constFunc = FuncDefn $ Func "const" [("a", TI32), ("b", TI32)] body TI32
  where body = EVal (VVar "a")

idFunc = FuncDefn $ Func "id" [("x", TI32)] body TI32
  where body = EVal (VVar "x")

idMaybeFunc = FuncDefn $ Func "idMaybe" [("x", TCon "MaybeInt")] body (TCon "MaybeInt")
  where body = EVal (VVar "x")

addMulFunc = FuncDefn $ Func "addMul" [("a", TI32), ("b", TI32), ("c", TI32)] body TI32
  where body = ELet (Just "d") TI32 (ECall "add" [VVar "a", VVar "b"]) TI32 $
               ELet (Just "e") TI32 (ECall "mul" [VVar "d", VVar "c"]) TI32 $
               ECall "id" [(VVar "e")]


maybeAddMulFunc = FuncDefn $ Func "maybeAddMul" [("may_a", TCon "MaybeInt"), ("may_b", TCon "MaybeInt"), ("may_c", TCon "MaybeInt")] body (TCon "MaybeInt")
  where body = EMatch (VVar "may_a") (TCon "MaybeInt")
                      [ ( "Nothing", [], EOp $ MemOp $ ConstrOp (Constr "Nothing" []))
                      , ( "Just"   , [Just "a"],
                              EMatch (VVar "may_b") (TCon "MaybeInt")
                                    [ ("Nothing", []        , EOp $ MemOp $ ConstrOp (Constr "Nothing" []))
                                    , ("Just"   , [Just "b"],
                                          EMatch (VVar "may_c") (TCon "MaybeInt")
                                                [ ("Nothing", [], EOp $ MemOp $ ConstrOp (Constr "Nothing" []))
                                                , ("Just", [Just "c"],
                                                    ELet (Just "d") TI32 (ECall "addMul" [VVar "a", VVar "b", VVar "c"]) (TCon "MaybeInt")
                                                          (EOp $ MemOp $ ConstrOp (Constr "Just" [VVar "d"]))       
                                                  )
                                                ]
                                      )
                                    ]
                                    
                            )
                      ]

mainFunc = FuncDefn $ Func "main" [("argc", TI32), ("argv", TPtr (TPtr (TI8)))] body TI32
  where body = ELet (Just "hello") TString (EVal $ VString "Hello World") TI32 $
               ELet (Just "five") TString (EVal $ VString "5") TI32 $
               ELet (Just "may_5_ptr") (TPtr $ TCon "MaybeInt") (ECall "just5" []) TI32 $
               ELet (Just "may_5") (TCon "MaybeInt") (EOp $ PtrOp $ DerefOp (TCon "MaybeInt") (VVar "may_5_ptr")) TI32 $
               ELet (Just "may_not") (TCon "MaybeInt") (ECall "nothing" []) TI32 $
               EMatch (VVar "may_5") TI32
                      [ ("Nothing", [], ECall "puts" [VVar "hello"])
                      , ("Just", [Just "c"], ECall "puts" [VVar "five"])       
                      ]

testSource :: [Defn]
testSource = [ mallocExtern
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


sampleModule :: AST.Module
sampleModule = genModule envEmpty testSource


main :: IO ()
main = LLVM.withContext $ \c -> LLVM.withModuleFromAST c sampleModule (LLVM.writeLLVMAssemblyToFile (LLVM.File "test.ll"))