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

import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import LLVM.AST hiding (function)
import LLVM.AST.Type as Ty
import LLVM.AST.Typed as Ty
import LLVM.AST.Operand as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import qualified LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import System.IO.Unsafe

log :: String -> a -> a
log msg a = unsafePerformIO (putStrLn msg) `seq` a


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
genModule env (LL.Module n defns)
  = buildModule "exampleModule"
              $ foldM genDefn env defns


genDefn :: (MonadFix m, MonadModuleBuilder m) => Env-> LL.Defn -> m Env
genDefn env = \case
  LL.FuncDefn f@(LL.Func n ps body@(LL.EType _ retty)) -> do 
    let paramtys = [ty | LL.PType _ ty <- ps]
        ty = genType env (LL.TFunc retty (NE.fromList paramtys))
    f' <- genFunc env f
    return $ envInsertFunc n f'
           $ envInsertType n ty env
  
  LL.FuncDefn f@(LL.Func n _ _) ->
    error $ "Codegen error: Function body is untyped for \'" ++ n ++ "\'."

  LL.ExternDefn ex@(LL.Extern n _ ty) -> do
    ex' <- genExtern env ex
    return $ envInsertFunc n ex'
          $ envInsertType n (genType env ty) env

  LL.DataTypeDefn dt@(LL.DataType n _) -> do
    let s = LL.sizeDataType (envSizes env) dt
    log ("Generating datatype " ++ n ++ " with size " ++ show s) $ return ()
    (ty_n, ty, cons) <- genDataType env s dt
    return . (\env -> foldl' (\env (_, con_ty, _, mems) -> envInsertMembers con_ty mems env) env cons)
           . envInsertSize ty_n s
           . envInsertTypeDef n ty
           . envInsertConstrs [(ty_n, ty, con_n, con_ty, ty_params) | (con_n, con_ty, ty_params, _) <- cons]
           $ env

genFunc :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Func -> m Operand
genFunc env (LL.Func name args body@(LL.EType _ retty)) = do
  let paramtys = [genType env ty | (LL.PType _ ty) <- args]
      params = map (, NoParameterName) paramtys
  function (str2Name name) params (genType env retty) $ \args' -> do
    entry <- block `named` "entry"
    args'' <- mapM genFuncParam (zip paramtys args')
    env' <- genPatExtractMany env (zip args args'')
    genBody env' body

genFuncParam :: (MonadFix m, MonadModuleBuilder m) => (Type, Operand) -> IRBuilderT m Operand
genFuncParam (ty, op) = do
  ptr <- alloca ty Nothing 4
  store ptr 4 op
  return ptr

genExtern :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Extern -> m Operand
genExtern env (LL.Extern name argtys retty) =
  extern (str2Name name) (genType env <$> argtys) (genType env retty)

genDataType :: (MonadFix m, MonadModuleBuilder m) => Env -> Int -> LL.DataType -> m (String, Type, [(String, Type, [Type], [Maybe String])])
genDataType env s dt@(LL.DataType n cons) = do
  ty <- typedef (str2Name n) (Just $ StructureType False [i8, ArrayType (fromIntegral s) i8] )
  cons' <- mapM (genDataTypeCon env n) cons
  return (n, ty, cons')

genDataTypeCon :: (MonadFix m, MonadModuleBuilder m) => Env -> String -> (String, [(Maybe String, LL.Type)]) -> m (String, Type, [Type], [Maybe String])
genDataTypeCon env dt_name (con_name, ty_params) = do
  let n = dt_name ++ "_" ++ con_name
      ty_params' = (genType env . snd) <$> ty_params
  (con_name,,,) <$> typedef (str2Name n) (Just $ StructureType False (i8 : ty_params') )
               <*> pure ty_params'
               <*> pure (fst <$> ty_params)

genType :: Env -> LL.Type -> Type
genType env = \case
  LL.TCon n ->
      case envLookupTypeDef n env of
        Nothing -> error $ "undefined type encountered: " ++ n
        Just ty -> ty
  LL.TPtr t -> ptr (genType env t)
  LL.TI8 -> i8
  LL.TI32 -> i32
  LL.TString -> ptr i8
  LL.TVoid -> Ty.void

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

  LL.ECall f@(LL.EType _ (LL.TFunc retty _)) xs -> do
    f' <- genExp env f
    xs' <- mapM (loadExp env) (NE.toList xs)
    r <- call f' [(x, []) | x <- xs']
    r_ptr <- alloca (genType env retty) Nothing 4
    store r_ptr 4 r
    return r_ptr

    {-
    case envLookupFunc f env of
      Nothing -> error $ "missing function declaration: " ++ f
      Just f' ->
        case envLookupType f env of
          Nothing -> error $ "missing function type: " ++ f
          Just VoidType -> do
            xs' <- mapM (\x -> genValue env x >>= (\arg -> load arg 4)) xs
            call f' [(x, []) | x <- xs']
          Just retty -> do
            xs' <- mapM (\x -> genValue env x >>= (\arg -> load arg 4)) xs
            r <- alloca retty Nothing 4
            op <- call f' [(x, []) | x <- xs']
            store r 4 op
            return r
      -}

  LL.ECall f xs -> error $ "genExp - Expected a typed function.\n\n"
                       ++ show f ++ " " ++ show xs ++ "\n\n"

  LL.EType e rty' ->
    genExp' env rty' e

  LL.ELet qs body -> do
    let go env (p, e) = do
          op <- log ("genELet is generating e: " ++ show e) $ genExp env e
          log ("genElet extracting pattern: " ++ show (p,op)) $ genPatExtract env p op
    env' <- foldM go env (NE.toList qs)
    genExp env' body

  LL.EIf p thenb elseb -> do
    error "Codegen: If unimplemented"

  LL.EMatchI i brs -> do
    error "Codegen: MatchI unimplemented"

  LL.EMatch s brs -> mdo
    s' <- genExp env s
    tag_ptr <- log ("EMatch accessing gep: " ++ show s') 
            $ gep s' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
    tag <- load tag_ptr 4

    let ty = genType env rty
    res <- alloca ty Nothing 4

    switch tag err_blk cases
    cases <- mapM (genCase env s' res exit_blk) (NE.toList brs)

    err_blk <- block `named` "case_failure"
    br exit_blk
    
    exit_blk <- block `named` "switch_exit"
    return res

  LL.ERef e -> genExp env e  -- We already keep everything in pointers

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
      Just (_, ty, con_ty, i, _) -> do
        log ("Generating constructor: " ++ show (LL.ECon n args)) $ return ()
        ptr <- alloca ty Nothing 4
        
        tag <- log ("ECon accessing gep: " ++ show ptr ++ "\n\ttype: " ++ show ty)
             $ gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
        store tag 4 (ConstantOperand $ C.Int 8 (toInteger i))

        con_ptr <- bitcast ptr (Ty.ptr con_ty)
        log ("bitcasted con_ptr: " ++ show con_ptr ++ "\n\ttype: " ++ show con_ty) $ return ()
        mapM_ (genConstrArg env con_ptr) (zip args [1..])

        log ("construtor generated") $ return ()

        return ptr


  LL.ENewCon n args -> do
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (ty_n, ty, con_ty, i, _) ->
        case envLookupSize ty_n env of
          Nothing -> error $ "unsized constructor: " ++ n
          Just s -> do
            let f = envLookupFunc "malloc" env
            r <- call f [(ConstantOperand $ C.Int 32 (toInteger $ s+1), [])]
            i8_ptr_ptr <- log ("ENewCon allocating ptr_i8 for r: " ++ show r) $ alloca (Ty.typeOf r) Nothing 4
            log ("ENewCon storing ptr_i8: " ++ show i8_ptr_ptr) $ store i8_ptr_ptr 4 r

            ptr_ptr <- bitcast i8_ptr_ptr (Ty.ptr $ Ty.ptr ty)
            ptr <- load ptr_ptr 4
            tag_ptr <- log ("ENewCon accessing gep: " ++ show ptr)
                     $ gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
            log ("ENewCon storing tag_ptr: " ++ show tag_ptr)
                $ store tag_ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))

            con_ptr <- bitcast ptr (Ty.ptr con_ty)
            log ("ENewCon Mapping over constructor args: " ++ show con_ptr)
              $ mapM_ (genConstrArg env con_ptr) (zip args [1..])

            log ("ENewCon successful: " ++ show ptr_ptr) $ return ptr_ptr

  LL.EFree e -> do
    let f = envLookupFunc "free" env
    e' <- genExp env e
    call f [(e', [])]

  LL.EGet e n -> do
    case envLookupMember n env of
      Nothing -> error $ "unrecognized member name: " ++ n
      Just (con_ty, i)  -> do
        e_ptr' <- genExp env e
        h_ptr <- bitcast e_ptr' (Ty.ptr con_ty)
        log "EGet accessing gep" $ gep h_ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]

  LL.EGetI e i -> do
    e_ptr' <- genExp env e
    i_ptr' <- genExp env i
    i' <- load i_ptr' 4
    log "EGetI accessing gep" $ gep e_ptr' [ConstantOperand $ C.Int 32 0, i']

  LL.ESet lhs rhs -> do
    lhs_ptr' <- genExp env lhs
    rhs_ptr' <- genExp env rhs

    rhs' <- load rhs_ptr' 4
    store lhs_ptr' 4 rhs'
    return lhs_ptr'


  LL.ENewArray xs -> undefined
  LL.ENewArrayI i -> undefined
  LL.EResizeArray e i -> undefined
  LL.EArrayElem e i -> undefined

  LL.ENewString str -> undefined 
  LL.ENewStringI i -> undefined

  LL.EOp op ->
    genOp env op

  e -> error $ "genExp - Unsupported expression:\n\n"
            ++ show e ++ "\n\n"


allocateVar :: (MonadFix m, MonadModuleBuilder m) => Env -> (Maybe String) -> LL.Exp -> IRBuilderT m Env
allocateVar env Nothing e = do
  _ <- genExp env e
  return env

allocateVar env (Just n) e@(LL.EType _ ty) = do
  let ty' = genType env ty
  case ty' of
    VoidType -> allocateVar env Nothing e
    _        -> do
      var_ptr <- alloca (genType env ty) Nothing 4

      let env' = envInsertType n ty'
               $ envInsertLocal n var_ptr env

      e_ptr' <- genExp env' e
      e' <- load e_ptr' 4
      store var_ptr 4 e'

      return env'


genConstrArg :: (MonadFix m, MonadModuleBuilder m) => Env -> Operand -> (LL.Exp, Int) -> IRBuilderT m ()
genConstrArg env ptr (arg, i) = do
  ptr' <- log ("genConstrArg accessing gep at " ++ show i ++ ": " ++ show ptr ++ "\n" ++ "\tArg: " ++ show arg)
        $ gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (fromIntegral i)]
  log ("genConstrArg gep successful: " ++ show ptr') $ return ()
  arg' <- loadExp env arg
  log ("genConstrArg arg loaded: " ++ show arg') $ return ()
  store ptr' 4 arg'

  log ("genConstrArg completed") $ return ()


genLit :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Type -> LL.Lit -> IRBuilderT m Operand
genLit env ty = \case
  LL.LInt i ->
    case ty of
      LL.TI32 -> do
        ptr <- alloca i32 Nothing 4
        store ptr 4 (ConstantOperand $ C.Int 32 (toInteger i))
        return ptr
      
      LL.TI8 -> do
        ptr <- alloca i32 Nothing 4
        store ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))
        return ptr

      _ -> error $ "Expected integer type, found: " ++ show ty

  LL.LString str -> do
    str_gptr <- globalStringPtr str =<< freshName "gstring"
    str_gptr' <- log ("genLit generated string: " ++ show str_gptr)
              $ bitcast str_gptr (Ty.ptr i8)
    val_ptr <- alloca (Ty.ptr i8) Nothing 4
    store val_ptr 4 str_gptr'
    return val_ptr
  
  LL.LGetI e i -> do
    e_ptr' <- genExp env e
    log ("LGetI accessing gep") $ gep e_ptr' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]



genOp :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Op -> IRBuilderT m Operand
genOp env = \case
  LL.OpAddI a b -> genBinaryOp env add a b
  LL.OpSubI a b -> genBinaryOp env sub a b
  LL.OpMulI a b -> genBinaryOp env mul a b

  LL.OpAddF a b -> genBinaryOp env fadd a b
  LL.OpSubF a b -> genBinaryOp env fsub a b
  LL.OpMulF a b -> genBinaryOp env fmul a b


genBinaryOp :: (MonadFix m, MonadModuleBuilder m)
            => Env -> (Operand -> Operand -> IRBuilderT m Operand) -> LL.Exp -> LL.Exp -> IRBuilderT m Operand
genBinaryOp env instr a@(LL.EType _ ty) b = do
    a' <- loadExp env a 
    b' <- loadExp env b
    
    ptr <- alloca (genType env ty) Nothing 4
    op <- instr a' b'
    store ptr 4 op
    return ptr


-----------------------------------------------------------------------
-- Pattern variable extraction
-----------------------------------------------------------------------

genPatExtractMany :: (MonadFix m, MonadModuleBuilder m) => Env -> [(LL.Pat, Operand)] -> IRBuilderT m Env
genPatExtractMany = foldM (\env (p, op) -> genPatExtract env p op)

genPatExtract :: (MonadFix m, MonadModuleBuilder m) => Env -> LL.Pat -> Operand -> IRBuilderT m Env
genPatExtract env p op = case p of
  LL.PVar n -> do
    op' <- load op 4
    {- let ty = typeOf op'
    a <- alloca ty Nothing 4
    store a 4 op'
    -}
    return $ envInsertType n undefined
           $ envInsertLocal n op env

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
  LL.PType p _ -> genPatExtract env p op


-- Case Generation
genCase :: (MonadFix m, MonadModuleBuilder m)
        => Env       -- ^ Codegen Environment
        -> Operand   -- ^ Pointer to data to match on
        -> Operand   -- ^ Pointer to where result is stored
        -> Name      -- ^ Name of exit block
        -> LL.Clause -- ^ Clause to generate 
        -> IRBuilderT m (C.Constant, Name)  -- ^ Returns index of clause and name of block
genCase env op res exit_blk (LL.Clause n xs body) = do
  let p = LL.PCon n (maybe LL.PWild LL.PVar <$> xs)
  blk <- block `named` "case"
  env' <- genPatExtract env p op
  case envLookupConstr n env of
    Nothing -> error $ "undefined constructor: " ++ n
    Just (_, _, con_ty, i, ty_params) -> do
      body' <- loadExp env' body
      store res 4 body'
      br exit_blk
      return (C.Int 8 (toInteger i), blk)


-- Helpers for conversion from strings to names

str2Name :: String -> Name
str2Name = Name . str2sbs

str2Param :: String -> ParameterName
str2Param = ParameterName . str2sbs

str2sbs :: String -> ShortByteString
str2sbs = SBS.toShort . S8.pack