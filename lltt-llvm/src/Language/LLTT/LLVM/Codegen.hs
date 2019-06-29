{-# LANGUAGE OverloadedStrings,
             LambdaCase,
             RecursiveDo,
             TupleSections
  #-}
module Language.LLTT.LLVM.Codegen where

import qualified Language.LLTT.Syntax as LL

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Fix

import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import LLVM.AST hiding (function)
import LLVM.AST.Type as Ty
import LLVM.AST.Typed as Ty
import LLVM.AST.Operand as AST
import LLVM.AST.IntegerPredicate as AST
import LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import qualified LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Extra

import System.IO.Unsafe

log :: String -> a -> a
log msg a = a -- unsafePerformIO (putStrLn msg) `seq` a


data Env = Env { envTypes     :: Map String Type
               , envFuncs     :: Map String Operand
               , envLocals    :: Map String Operand
               , envSizes     :: Map String Int
               , envTypeDefs  :: Map String Type
               , envConstrs   :: Map String (String, Type, Type, Int, [Type])
               , envMembers   :: Map String (Type, Int)
               }

envEmpty :: Env
envEmpty = Env { envTypes = mempty
               , envFuncs = mempty
               , envLocals = mempty
               , envSizes = mempty 
               , envTypeDefs = mempty
               , envConstrs = mempty
               , envMembers = mempty
               }

envInsertTypes :: [(String, Type)] -> Env -> Env
envInsertTypes types env = foldl' go env types
  where go env (n, ty) = envInsertType n ty env 

envInsertType :: String -> Type -> Env -> Env
envInsertType n ty env = env { envTypes = Map.insert n ty (envTypes env) }

envLookupType :: String -> Env -> Maybe Type
envLookupType n env = Map.lookup n (envTypes env)


envInsertLocals :: [(String, Operand)] -> Env -> Env
envInsertLocals locals env = foldl' go env locals
  where go env (n, op) = envInsertLocal n op env 

envInsertLocal :: String -> Operand -> Env -> Env
envInsertLocal n op env = env { envLocals = Map.insert n op (envLocals env) }

envLookupLocal :: String -> Env -> Maybe Operand
envLookupLocal n env = Map.lookup n (envLocals env)


envInsertFunc :: String -> Operand -> Env -> Env
envInsertFunc n op env = env { envFuncs = Map.insert n op (envFuncs env) }

envLookupFunc :: String -> Env -> Operand
envLookupFunc n env = 
  case Map.lookup n (envFuncs env) of
    Just op -> op
    Nothing -> error $ "envLookupFunc - Couldn't find function " ++ n


envInsertSize :: String -> Int -> Env -> Env
envInsertSize n s env = env { envSizes = Map.insert n s (envSizes env) }

envLookupSize :: String -> Env -> Maybe Int
envLookupSize n env = Map.lookup n (envSizes env)

envInsertTypeDef :: String -> Type -> Env -> Env
envInsertTypeDef n ty env = env { envTypeDefs = Map.insert n ty (envTypeDefs env) }

envLookupTypeDef :: String -> Env -> Maybe Type
envLookupTypeDef n env = Map.lookup n (envTypeDefs env)


envInsertConstrs :: [(String, Type, String, Type, [Type])] -> Env -> Env
envInsertConstrs ns env = foldl' go env (zip ns [0..])
  where go env ((ty_n, ty, con_n, con_ty, ty_params), i)
          = envInsertConstr con_n ty_n ty con_ty i ty_params env 

envInsertConstr :: String -> String -> Type -> Type -> Int -> [Type] -> Env -> Env
envInsertConstr con_n ty_n ty con_ty i argty env
  = env { envConstrs = Map.insert con_n (ty_n, ty, con_ty, i, argty) (envConstrs env) }

envLookupConstr :: String -> Env -> Maybe (String, Type, Type, Int, [Type])
envLookupConstr con_n env = Map.lookup con_n (envConstrs env)

envInsertMembers :: Type -> [Maybe String] -> Env -> Env
envInsertMembers con_ty mem_ns env = foldl' go env (zip mem_ns [1..])
  where go env' (Just mem_n, i) = envInsertMember mem_n con_ty i env'
        go env' (Nothing, _) = env'

envInsertMember :: String -> Type -> Int -> Env -> Env
envInsertMember mem_n con_ty i env
  = env { envMembers = Map.insert mem_n (con_ty, i) (envMembers env) }

envLookupMember :: String -> Env -> Maybe (Type, Int)
envLookupMember mem_n env = Map.lookup mem_n (envMembers env)


genModule :: Env -> LL.Module -> Module
genModule env1 (LL.Module n defns)
  = buildModule "exampleModule" $ mdo
      env2 <- foldM genDecl env1 defns
      ops <- mapM (genFunc env3) [f | (LL.FuncDefn f) <- defns]
      let env3 = foldl' (\env (n, f, ty) ->
                              envInsertFunc n f
                            $ envInsertType n ty env) env2 ops
      return ()

genFunc :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Func -> m (String, Operand, Type)
genFunc env (LL.Func name args body@(LL.EType _ retty)) = do
  let paramtys = [ty | LL.PType _ ty <- args]
      paramtys' = genType env <$> paramtys
      params = map (, NoParameterName) paramtys'
      ty = genType env (LL.TFunc retty (NE.fromList paramtys))
  f' <- function (str2Name name) params (genType env retty) $ \args' -> do
    entry <- block `named` "entry"
    args'' <- mapM genFuncParam (zip paramtys' args')
    env' <-  genPatExtractMany env (zip args args'')
    genBody env' body
  return (name, f', ty)

genFunc env (LL.Func n _ _) =
  error $ "Codegen error: Function body is untyped for \'" ++ n ++ "\'."

genDecl :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Defn -> m Env
genDecl env = \case
  {-
  LL.FuncDefn (LL.Func n args (LL.EType _ retty))
    | n == "main" -> return env
    | otherwise -> do
        let argtys = [ty | (LL.PType _ ty) <- args]
        f' <- extern (str2Name n) (genType env <$> argtys) (genType env retty)
        return $ envInsertFunc n f'
              $ envInsertType n (genType env (LL.TFunc retty (NE.fromList argtys)))
              $ env

  LL.FuncDefn (LL.Func n _ _) ->
    error $ "Codegen error: Function body is untyped for \'" ++ n ++ "\'."
    -}

  LL.ExternDefn ex@(LL.Extern n paramtys retty) -> do
    ex' <- genExtern env ex
    return $ envInsertFunc n ex'
           $ envInsertType n (genType env (LL.TFunc retty (NE.fromList paramtys)))
           $ env
  
  LL.DataTypeDefn dt@(LL.DataType n _) -> do
    let s = LL.sizeDataType (envSizes env) dt
    log ("Generating datatype " ++ n ++ " with size " ++ show s) $ return ()
    (ty_n, ty, cons) <- genDataType env s dt
    return . (\env -> foldl' (\env (_, con_ty, _, mems) -> envInsertMembers con_ty mems env) env cons)
           . envInsertSize ty_n s
           . envInsertTypeDef n ty
           . envInsertConstrs [(ty_n, ty, con_n, con_ty, ty_params) | (con_n, con_ty, ty_params, _) <- cons]
           $ env

  _ -> return env

genFuncParam :: (MonadFix m, MonadModuleBuilder m) => (Type, Operand) -> IRBuilderT m Operand
genFuncParam (ty, op) = do
  ptr <- alloca ty Nothing 4
  store ptr 4 op
  return ptr

genVars :: (MonadFix m, MonadModuleBuilder m) => Env -> [(String, Type)] -> IRBuilderT m Env
genVars = foldM genVar

genVar :: (MonadFix m, MonadModuleBuilder m) => Env -> (String, Type) -> IRBuilderT m Env
genVar env (n, ty) = do
  ptr <- alloca ty Nothing 4
  return $ envInsertLocal n ptr
         $ envInsertType n ty
         $ env

genExtern :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Extern -> m Operand
genExtern env (LL.Extern name argtys retty) =
  extern (str2Name name) (genType env <$> argtys) (genType env retty)

genDataType :: (MonadFix m, MonadModuleBuilder m) => Env -> Int -> LL.DataType -> m (String, Type, [(String, Type, [Type], [Maybe String])])
genDataType env s dt@(LL.DataType n cons) = do
  ty <- typedef (str2Name n) (Just $ StructureType False [i8, ArrayType (fromIntegral s) i8] )
  let env' = envInsertTypeDef n ty env
  cons' <- mapM (genDataTypeCon env' n) cons
  return (n, ty, cons')

genDataTypeCon :: (MonadFix m, MonadModuleBuilder m) => Env -> String -> (String, [(Maybe String, LL.Type)]) -> m (String, Type, [Type], [Maybe String])
genDataTypeCon env dt_name (con_name, ty_params) = do
  let n = dt_name ++ "_" ++ con_name
      ty_params' = (genType env . snd) <$> ty_params
  (con_name,,,) <$> typedef (str2Name n) (Just $ StructureType True (i8 : ty_params') )
                <*> pure ty_params'
                <*> pure (fst <$> ty_params)

genType :: Env -> LL.Type -> Type
genType env = \case
  LL.TVar n -> error "LLTT doesn't support type variables yet!"
  LL.TCon n ->
      case envLookupTypeDef n env of
        Nothing -> error $ "undefined type encountered: " ++ n
        Just ty -> ty
  LL.TI8 -> i8
  LL.TI32 -> i32
  LL.TI64 -> i64
  LL.TF32 -> float
  LL.TF64 -> double
  LL.TChar -> i8
  LL.TArray i ty -> ArrayType (fromIntegral i) (genType env ty)
  LL.TBool -> i1
  LL.TPtr t -> Ty.ptr (genType env t)
  LL.TString -> Ty.ptr i8
  LL.TVoid -> Ty.void
  LL.TFunc retty paramtys -> Ty.FunctionType (genType env retty)
                                             (genType env <$> NE.toList paramtys)
                                             False

genBody :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Exp -> IRBuilderT m ()
genBody env body = do
  body' <- genExp env body
  r <- load body' 4
  ret r


loadExp :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Exp -> IRBuilderT m Operand
loadExp env e = do
  e_ptr <- genExp env e
  load e_ptr 4


genExp :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Exp -> IRBuilderT m Operand
genExp env e@(LL.EType _ ty) = do
  op <- log ("Generating expression " ++ show e)
                                  $ genExp' env ty e
  log ("Exp gen successful: " ++ show op) $ return op

genExp' :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Type -> LL.Exp -> IRBuilderT m Operand
genExp' env rty = \case
  LL.EVar n -> case envLookupLocal n env of
    Nothing -> do
      log ("looking up var " ++ n ++ ", found Nothing.") $ return ()
      let f = envLookupFunc n env 
      case Ty.typeOf f of
        Ty.PointerType (Ty.FunctionType retty args _) _
          | length args == 0 -> do
                val_ptr <- alloca retty Nothing 4
                retval <- call f []
                store val_ptr 4 retval
                return val_ptr

          | otherwise -> return f
        
        ty -> error $ "\n\nExpected function to be stored as a pointer, found:\n\n"
                   ++ show ty ++ "\n\n"

    Just local -> do

      log ("looking up var " ++ n ++ ", found local " ++ show local) $ return ()
      case Ty.typeOf local of
        Ty.PointerType (Ty.FunctionType retty args _) _
          | length args == 0 -> do
                val_ptr <- alloca retty Nothing 4
                local' <- load local 4
                retval <- call local []
                store val_ptr 4 retval
                return val_ptr

          | otherwise -> return local
        
        _ -> return local


  LL.ELit l -> genLit env rty l

  LL.ECall f@(LL.EType _ (LL.TFunc retty paramtys)) xs -> mdo

    br call_init
    call_init <- block `named` "call.init"
    r_ptr <- alloca (genType env retty) Nothing 4
    f' <- genExp env f
    xs' <- mapM (loadExp env) (NE.toList xs)
    br call_app

    call_app <- block `named` "call.app"
    r <- call f' [(x, []) | x <- xs']
    store r_ptr 4 r
    return r_ptr

  LL.ECall f xs -> error $ "genExp - Expected a typed function.\n\n"
                       ++ show f ++ " " ++ show xs ++ "\n\n"

  LL.EType e rty' ->
    genExp' env rty' e

  LL.ELet qs body -> mdo
    br let_header
    let_header <- block `named` "let.header"
    
    let (ps, es) = unzip $ NE.toList qs
    env' <- foldM genPatAlloc env ps

    let go (p, e) = mdo
          br rhs_blk

          rhs_blk <- block `named` "let.header.rhs"
          op <- log ("genELet is generating e: " ++ show e) $ genExp env' e
          br lhs_init_blk

          lhs_init_blk <- block `named` "let.header.lhs.init"
          genPatInit env' p op

    mapM_ go (NE.toList qs)
    br let_body

    let_body <- block `named` "let.body"
    genExp env' body

  LL.EIf p thenB elseB -> mdo
    let ty = genType env rty
    res <- alloca ty Nothing 4

    p_ptr' <- genExp env p
    p' <- load p_ptr' 4


    condBr p' then_blk else_blk

    then_blk <- block `named` "then"
    thenB_ptr' <- genExp env thenB
    thenB' <- load thenB_ptr' 4
    store res 4 thenB'
    br ifexit_blk

    else_blk <- block `named` "else"
    elseB_ptr' <- genElse env elseB
    elseB' <- load elseB_ptr' 4
    store res 4 elseB'
    br ifexit_blk

    ifexit_blk <- block `named` "ifexit"
    return res


  LL.EMatchI i brs -> do
    error "Codegen: MatchI unimplemented"

  LL.EMatch s brs -> mdo
    let ty = genType env rty
    res <- alloca ty Nothing 4

    s' <- genExp env s
    tag_ptr <- log ("EMatch accessing gep: " ++ show s') 
            $ gep s' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
    tag <- load tag_ptr 4

    switch tag err_blk cases
    cases <- mapM (genCase env s' res exit_blk) (NE.toList brs)

    err_blk <- block `named` "case_failure"
    br exit_blk
    
    exit_blk <- block `named` "switch_exit"
    return res

  LL.ERef e -> do
    r_ptr <- alloca (genType env rty) Nothing 4
    e_ptr' <- genExp env e  -- We already keep everything in pointers
    store r_ptr 4 e_ptr'
    return r_ptr

  -- To deref, assume that e generates an operand which is
  -- a pointer to a pointer. We simply load the pointer,
  -- because this gives us a pointer to a value, which is
  -- basically the raw value in llvm.
  LL.EDeref e -> do
    e_ptr_ptr <- log ("EDeref genExp on e: " ++ show e)
               $ genExp env e 
    load e_ptr_ptr 4

  LL.ECon n args ->
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (_, ty, con_ty, i, _) -> mdo
        br constr_load_blk

        constr_load_blk <- block `named` "constr.load"
        args' <- mapM (genExp env) args
        br constr_init_blk
        
        constr_init_blk <- block `named` "constr.init"
        ty_ptr <- alloca ty Nothing 4
        
        tag_ptr <- gep ty_ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
        store tag_ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))

        con_ptr <- bitcast ty_ptr (Ty.ptr con_ty)
        mapM_ (genConstrArg env con_ptr) (zip args' [1..])

        return ty_ptr


  LL.ENewCon n args -> do
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (ty_n, ty, con_ty, i, _) ->
        case envLookupSize ty_n env of
          Nothing -> error $ "unsized constructor: " ++ n
          Just s -> mdo
            br constr_load_blk

            constr_load_blk <- block `named` "newconstr.load"
            args' <- mapM (genExp env) args
            br constr_init_blk

            i8_ptr_ptr <- alloca (Ty.ptr Ty.i8) Nothing 4
            let f = envLookupFunc "malloc" env
            r <- call f [(ConstantOperand $ C.Int 32 (toInteger $ s+1), [])]
            store i8_ptr_ptr 4 r
            br constr_init_blk

            constr_init_blk <- block `named` "newconstr.init"
            ptr_ptr <- bitcast i8_ptr_ptr (Ty.ptr $ Ty.ptr ty)
            ptr <- load ptr_ptr 4
            tag_ptr <- gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
            store tag_ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))

            con_ptr <- bitcast ptr (Ty.ptr con_ty)
            mapM_ (genConstrArg env con_ptr) (zip args' [1..])

            return ptr_ptr

  LL.EFree e -> do
    let f = envLookupFunc "free" env
    e' <- loadExp env e
    call f [(e', [])]

  LL.EGet e n -> do
    case envLookupMember n env of
      Nothing -> error $ "unrecognized member name: " ++ n
      Just (con_ty, i)  -> do
        e_ptr' <- genExp env e
        h_ptr <- bitcast e_ptr' (Ty.ptr con_ty)
        log "EGet accessing gep" $ gep h_ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]

  LL.EGetI e i -> do
    e_ptr' <- loadExp env e
    i' <- loadExp env i
    gep e_ptr' [i']

  LL.ESet lhs rhs -> do
    lhs_ptr' <- genExp env lhs
    rhs' <- loadExp env rhs
    store lhs_ptr' 4 rhs'
    return lhs_ptr'


  LL.ENewArray xs -> mdo
    br newarr_stage_blk
    newarr_stage_blk <- block `named` "newarray.stage"
    let ty = LL.exType $ head xs 
    x_ptrs' <- mapM (genExp env) xs
    br newarr_alloc_blk

    newarr_alloc_blk <- block `named` "newarray.alloc"
    i8_ptr_ptr <- alloca (Ty.ptr Ty.i8) Nothing 4
    arr_ptr_ptr <- bitcast i8_ptr_ptr (Ty.ptr $ genType env rty)
    br newarr_call_blk

    newarr_call_blk <- block `named` "newarray.call"
    let f = envLookupFunc "malloc" env
        s = LL.sizeType (envSizes env) ty 
    xs' <- mapM (`load` 4) x_ptrs'
    r <- call f [(ConstantOperand $ C.Int 32 (toInteger $ s * length xs), [])]
    
    store i8_ptr_ptr 4 r
    arr_ptr <- load arr_ptr_ptr 4
    mapM_ (\(x, i) -> do 
                  e <- gep arr_ptr [ConstantOperand $ C.Int 32 (toInteger i)]
                  store e 4 x
          ) (zip xs' [0..])

    return arr_ptr_ptr


  LL.ENewArrayI i -> mdo
    br newstr_stage_blk
    newstr_stage_blk <- block `named` "newarray.stage"
    i_ptr' <- genExp env i
    br newstr_alloc_blk

    newstr_alloc_blk <- block `named` "newarray.alloc"
    i8_ptr_ptr <- alloca (Ty.ptr Ty.i8) Nothing 4
    arr_ptr_ptr <- bitcast i8_ptr_ptr (Ty.ptr $ genType env rty)
    br newstr_call_blk

    newstr_call_blk <- block `named` "newarray.call"
    let f = envLookupFunc "malloc" env
        s = LL.sizeType (envSizes env) rty 
    i' <- load i_ptr' 4
    n <- mul i' (ConstantOperand $ C.Int 32 (toInteger s))
    r <- call f [(n, [])]
    store i8_ptr_ptr 4 r

    return arr_ptr_ptr


  LL.EResizeArray e i -> undefined
  LL.EArrayElem e i -> undefined

  LL.ENewString str -> undefined
  LL.ENewStringI i -> mdo
    br newstr_stage_blk
    newstr_stage_blk <- block `named` "newstr.stage"
    i_ptr' <- genExp env i
    br newstr_alloc_blk

    newstr_alloc_blk <- block `named` "newstr.alloc"
    i8_ptr_ptr <- alloca (Ty.ptr Ty.i8) Nothing 4
    br newstr_call_blk

    newstr_call_blk <- block `named` "newstr.call"
    let f = envLookupFunc "malloc" env
    i' <- load i_ptr' 4
    r <- call f [(i', [])]
    store i8_ptr_ptr 4 r

    return i8_ptr_ptr


  LL.EOp op ->
    genOp env op

  e -> error $ "genExp - Unsupported expression:\n\n"
            ++ show e ++ "\n\n"



genConstrArg :: (MonadFix m, MonadModuleBuilder m) => Env -> Operand -> (Operand, Int) -> IRBuilderT m ()
genConstrArg env ptr (arg_ptr, i) = do
  eptr <- gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (fromIntegral i)]
  arg <- load arg_ptr 4
  store eptr 4 arg


genLit :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Type -> LL.Lit -> IRBuilderT m Operand
genLit env ty = \case
  LL.LInt i ->
    case ty of
      LL.TI8 -> do
        ptr <- alloca i8 Nothing 4
        store ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))
        return ptr

      LL.TI32 -> do
        ptr <- alloca i32 Nothing 4
        store ptr 4 (ConstantOperand $ C.Int 32 (toInteger i))
        return ptr
      
      LL.TI64 -> do
        ptr <- alloca i64 Nothing 4
        store ptr 4 (ConstantOperand $ C.Int 64 (toInteger i))
        return ptr

      _ -> error $ "Expected integer type, found: " ++ show ty

  LL.LDouble d -> error "double not supported yet"

  LL.LBool True -> do
    ptr <- alloca i1 Nothing 4
    store ptr 4 (ConstantOperand $ C.Int 1 1)
    return ptr

  LL.LBool False -> do
    ptr <- alloca i1 Nothing 4
    store ptr 4 (ConstantOperand $ C.Int 1 0)
    return ptr

  LL.LChar c -> do
    ptr <- alloca i8 Nothing 4
    store ptr 4 (ConstantOperand $ C.Int 8 (toInteger (ord c)))
    return ptr

  LL.LString str -> do
    val_ptr <- alloca (Ty.ptr i8) Nothing 4
    str_gptr <- globalStringPtr str =<< freshName "gstring"
    str_gptr' <- bitcast str_gptr (Ty.ptr i8)
    store val_ptr 4 str_gptr'
    return val_ptr
  
  LL.LStringI i -> do
    let ty = Ty.ArrayType (fromIntegral i) i8
    strptr <- alloca ty Nothing 4
    strptr_ptr <- alloca (Ty.ptr i8) Nothing 4
    strptr' <- bitcast strptr (Ty.ptr i8)
    store strptr_ptr 4 strptr'
    return strptr_ptr

  LL.LGetI e i -> do
    e_ptr' <- genExp env e
    log ("LGetI accessing gep") $ gep e_ptr' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]

genElse :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Else -> IRBuilderT m Operand
genElse env = \case
  LL.Else e -> genExp env e
  LL.Elif p t@(LL.EType _ ty) f -> genExp env (LL.EType (LL.EIf p t f) ty)

genOp :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Op -> IRBuilderT m Operand
genOp env = \case
  LL.OpAddI a b -> genBinaryOp env add LL.TI32 a b
  LL.OpSubI a b -> genBinaryOp env sub LL.TI32 a b
  LL.OpMulI a b -> genBinaryOp env mul LL.TI32 a b

  LL.OpAddF a b -> genBinaryOp env fadd LL.TI32 a b
  LL.OpSubF a b -> genBinaryOp env fsub LL.TI32 a b
  LL.OpMulF a b -> genBinaryOp env fmul LL.TI32 a b

  LL.OpEqI a b -> genBinaryOp env (icmp AST.EQ) LL.TBool a b
  LL.OpNeqI a b -> genBinaryOp env (icmp AST.NE) LL.TBool a b


genBinaryOp :: (MonadFix m, MonadModuleBuilder m)
            => Env -> (Operand -> Operand -> IRBuilderT m Operand) -> LL.Type -> LL.Exp -> LL.Exp -> IRBuilderT m Operand
genBinaryOp env instr retty a@(LL.EType _ ty) b = do
    a' <- loadExp env a 
    b' <- loadExp env b
    
    ptr <- alloca (genType env retty) Nothing 4
    op <- instr a' b'
    store ptr 4 op
    return ptr


-----------------------------------------------------------------------
-- Pattern variable extraction
-----------------------------------------------------------------------

genPatAlloc :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> IRBuilderT m Env
genPatAlloc env p = genVars env (second (genType env) <$> LL.patFreeTyped p)

genPatInit :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> Operand -> IRBuilderT m ()
genPatInit env p@(LL.PType _ ty) op = genPatInit' env p ty op

genPatInit' :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> LL.Type -> Operand -> IRBuilderT m ()
genPatInit' env p ty op = case p of
  LL.PVar n -> do
    case envLookupLocal n env of
      Nothing -> error $ "genPatInit - Unallocated pattern variable encountered: " ++ show p
      Just val_ptr -> do
        op' <- load op 4
        store val_ptr 4 op'

  LL.PCon n ps -> do
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (_, _, con_ty, i, _) -> do
        con_op <- bitcast op (Ty.ptr $ con_ty)
        let go (p, i) = do
              arg_op <- log "PCon accessing gep" $ gep con_op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
              genPatInit env p arg_op
        mapM_ go (zip ps [1..])

  LL.PWild -> return ()

  LL.PType p LL.TVoid -> return ()
  LL.PType p ty' -> genPatInit' env p ty' op

genPatExtractMany :: (MonadFix m, MonadModuleBuilder m) => Env -> [(LL.Pat, Operand)] -> IRBuilderT m Env
genPatExtractMany = foldM (\env (p, op) -> genPatExtract env p op)

genPatExtract :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> Operand -> IRBuilderT m Env
genPatExtract env p@(LL.PType _ pty) op = genPatExtract' env p pty op
genPatExtract _ p _ = error $ "Expected typed expression, found: " ++ show p

genPatExtract' :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> LL.Type -> Operand -> IRBuilderT m Env
genPatExtract' env p pty op = case p of
  LL.PVar n -> do
    let ty' = genType env pty
    return $ envInsertType n ty'
           $ envInsertLocal n op
           $ env

  LL.PCon n ps -> do
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (_, _, con_ty, i, _) -> do
        con_op <- bitcast op (Ty.ptr $ con_ty)
        let go (env, i) p = do
              op' <- log "PCon accessing gep" $ gep con_op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
              env' <- genPatExtract env p op'
              return (env', i+1)
        fst <$> foldM go (env, 1) ps

  LL.PWild -> return env

  LL.PType p LL.TVoid -> return env
  LL.PType p pty' -> genPatExtract' env p pty' op


-- Case Generation
genCase :: (MonadFix m, MonadModuleBuilder m)
        => Env       -- ^ Codegen Environment
        -> Operand   -- ^ Pointer to data to match on
        -> Operand   -- ^ Pointer to where result is stored
        -> Name      -- ^ Name of exit block
        -> LL.Clause -- ^ Clause to generate 
        -> IRBuilderT m (C.Constant, Name)  -- ^ Returns index of clause and name of block
genCase env op res exit_blk (LL.Clause n xs body) = do
  case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (_, _, con_ty, i, arg_tys) -> mdo
        case_alloc_blk <- block `named` "case.alloc"
        env' <- genVars env [ (x,ty) | (Just x, ty) <- zip xs arg_tys ]
        br case_init_blk

        case_init_blk <- block `named` "case.init"
        con_op <- bitcast op (Ty.ptr $ con_ty)
        let go (Nothing, _, _) = return ()
            go (Just x, ty, i) = do
              case envLookupLocal x env' of
                Nothing -> error $ "genCase - Expected pattern variable to be in scope: " ++ show x
                Just a_ptr -> do
                  elem_ptr' <- log "genCase.go accessing gep" $ gep con_op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
                  elem' <- load elem_ptr' 4
                  store a_ptr 4 elem'
        mapM_ go (zip3 xs arg_tys [1..])
        br case_body_blk

        case_body_blk <- block `named` "case.body"
        body' <- loadExp env' body
        store res 4 body'
        br exit_blk

        return (C.Int 8 (toInteger i), case_alloc_blk)


-- Helpers for conversion from strings to names

str2Name :: String -> Name
str2Name = Name . str2sbs

str2Param :: String -> ParameterName
str2Param = ParameterName . str2sbs

str2sbs :: String -> ShortByteString
str2sbs = SBS.toShort . S8.pack