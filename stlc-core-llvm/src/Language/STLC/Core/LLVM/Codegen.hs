{-# LANGUAGE OverloadedStrings,
             LambdaCase,
             RecursiveDo,
             TupleSections
  #-}
module Language.STLC.Core.LLVM.Codegen where

import qualified Language.STLC.Core.Syntax as Core

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
import LLVM.AST.Type as AST
import LLVM.AST.Operand as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import qualified LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction


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

envLookupFunc :: String -> Env -> Maybe Operand
envLookupFunc n env = Map.lookup n (envFuncs env)


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
  where go env ((ty_n, ty, con_n, con_ty, ty_params), i) = envInsertConstr con_n ty_n ty con_ty i ty_params env 

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


genModule :: Env -> [Core.Defn] -> Module
genModule env defns = buildModule "exampleModule" $
                        foldM genDefn env defns

genDefn :: (MonadFix m, MonadModuleBuilder m) => Env-> Core.Defn -> m Env
genDefn env = \case
  Core.FuncDefn f@(Core.Func n _ _ ty) -> do
    f' <- genFunc env f
    return $ envInsertFunc n f' $ envInsertType n (genType env ty) env

  Core.ExternDefn ex@(Core.Extern n _ ty) -> do
    ex' <- genExtern env ex
    return $ envInsertFunc n ex' $ envInsertType n (genType env ty) env

  Core.DataTypeDefn dt@(Core.DataType n _) -> do
    let s = Core.sizeDataType (envSizes env) dt
    (ty_n, ty, cons) <- genDataType env s dt
    return . (\env -> foldl' (\env (_, con_ty, _, mems) -> envInsertMembers con_ty mems env) env cons)
           . envInsertSize ty_n s
           . envInsertTypeDef n ty
           . envInsertConstrs [(ty_n, ty, con_n, con_ty, ty_params) | (con_n, con_ty, ty_params, _) <- cons]
           $ env

genFunc :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Func -> m Operand
genFunc env (Core.Func name args body retty) = mdo
  function (str2Name name) [ (genType env ty, str2Param n) | (n, ty) <- args] (genType env retty) $ \args' -> mdo
    entry <- block `named` "entry"
    locals <- mapM (uncurry genFuncParam) (zip ((genType env . snd) <$> args) args')
    let env' = envInsertLocals (zip (fst <$> args) locals) $ envInsertTypes [(n, genType env ty) | (n, ty) <- args] env
    genBody env' body

genFuncParam :: (MonadFix m, MonadModuleBuilder m) => Type -> Operand -> IRBuilderT m Operand
genFuncParam ty op = do
  ptr <- alloca ty Nothing 4
  store ptr 4 op
  return ptr

genExtern :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Extern -> m Operand
genExtern env (Core.Extern name argtys retty) =
  extern (str2Name name) (genType env <$> argtys) (genType env retty)

genDataType :: (MonadFix m, MonadModuleBuilder m) => Env -> Int -> Core.DataType -> m (String, Type, [(String, Type, [Type], [Maybe String])])
genDataType env s dt@(Core.DataType n cons) = do
  ty <- typedef (str2Name n) (Just $ StructureType False [i8, ArrayType (fromIntegral s) i8] )
  cons' <- mapM (genDataTypeCon env n) cons
  return (n, ty, cons')

genDataTypeCon :: (MonadFix m, MonadModuleBuilder m) => Env -> String -> (String, [(Maybe String, Core.Type)]) -> m (String, Type, [Type], [Maybe String])
genDataTypeCon env dt_name (con_name, ty_params) = do
  let n = dt_name ++ "_" ++ con_name
      ty_params' = [ genType env ty | (_, ty) <- ty_params]
  (con_name,,,) <$> typedef (str2Name n) (Just $ StructureType False (i8 : ty_params') )
               <*> pure ty_params'
               <*> pure (fst <$> ty_params)

genType :: Env -> Core.Type -> Type
genType env = \case
  Core.TCon n ->
      case envLookupTypeDef n env of
        Nothing -> error $ "undefined type encountered: " ++ n
        Just ty -> ty
  Core.TPtr t -> ptr (genType env t)
  Core.TI8 -> i8
  Core.TI32 -> i32
  Core.TString -> ptr i8
  Core.TVoid -> AST.void

genBody :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Exp -> IRBuilderT m ()
genBody env body = do
  body' <- genExp env body
  r <- load body' 4
  ret r

loadCallArg :: (MonadFix m, MonadModuleBuilder m) => Operand -> IRBuilderT m Operand
loadCallArg arg = load arg 4

loadExp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Exp -> IRBuilderT m Operand
loadExp env e = do
  e_ptr <- genExp env e
  load e_ptr 4


genExp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Exp -> IRBuilderT m Operand
genExp env = \case
  Core.ECall f@(Core.EType _ (Core.TFunc retty _)) xs -> do
    f' <- loadExp env f
    xs' <- mapM (loadExp env) xs
    r <- call f' [(x, []) | x <- xs']

    let retty' = genType env retty
    r_ptr <- alloca retty' Nothing 4
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

  Core.ELet vars body@(Core.EType _ t2) -> mdo
    let t2' = genType env t2

    env' <- foldM (\env (n, e) -> allocateVar env n e ) env vars
    genExp env' body


  Core.EMatch s brs -> mdo
    s' <- genExp env s
    tag_ptr <- gep s' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
    tag <- load tag_ptr 4

    let (_, _, Core.EType _ ty) = NE.head brs
        ty' = genType env ty
    res <- alloca ty' Nothing 4

    switch tag err_blk cases
    cases <- mapM (genCase env s' res exit_blk) (NE.toList brs)

    err_blk <- block `named` "case_failure"
    br exit_blk
    
    exit_blk <- block `named` "switch_exit"
    return res
    
  Core.EVal v ->
    genValue env v

  Core.EOp op ->
    genOp env op


allocateVar :: (MonadFix m, MonadModuleBuilder m) => Env -> (Maybe String) -> Core.Exp -> IRBuilderT m Env
allocateVar env Nothing e = do
  _ <- genExp env e
  return env

allocateVar env (Just n) e@(Core.EType _ ty) = do
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


genCase :: (MonadFix m, MonadModuleBuilder m) => Env -> Operand -> Operand -> Name -> (String, [Maybe String], Core.Exp) -> IRBuilderT m (C.Constant, Name)
genCase env par res exit_blk (n, xs, body) = do
  blk <- block `named` "case"
  case envLookupConstr n env of
    Nothing -> error $ "undefined constructor: " ++ n
    Just (_, _, con_ty, i, ty_params) -> do
      par' <- bitcast par (AST.ptr con_ty)
      case_locals <- catMaybes <$> mapM (genCaseLocal par') (zip3 ty_params xs [1..])
      
      let env' = envInsertTypes [(n, ty) | (n, ty, _) <- case_locals] $
                  envInsertLocals [(n, x) | (n, _, x) <- case_locals] env
      
      body_ptr' <- genExp env' body
      body' <- load body_ptr' 4
      store res 4 body'
      br exit_blk
      return (C.Int 8 (toInteger i), blk)


genCaseLocal :: (MonadFix m, MonadModuleBuilder m) => Operand -> (Type, Maybe String, Int) -> IRBuilderT m (Maybe (String, Type, Operand))
genCaseLocal _ (_, Nothing, _) = return Nothing
genCaseLocal par (ty, Just n, i) = do
  local <- gep par [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]
  return $ Just (n, ty, local)


genOp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Op -> IRBuilderT m Operand
genOp env = \case
  Core.IArithOp op -> do
    ptr <- alloca i32 Nothing 4
    op' <- genIArithOp env op
    store ptr 4 op'
    return ptr

  Core.FArithOp op -> undefined
  Core.PtrOp    op -> genPtrOp env op
  Core.MemOp    op -> genMemOp env op
  Core.ArrayOp  op -> undefined

genIArithOp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.IArithOp -> IRBuilderT m Operand
genIArithOp env = \case
  Core.AddOpI a b -> do
    a_ptr' <- genExp env a 
    b_ptr' <- genExp env b
    a' <- load a_ptr' 4
    b' <- load b_ptr' 4
    add a' b'

  Core.SubOpI a b -> do
    a_ptr' <- genExp env a 
    b_ptr' <- genExp env b
    a' <- load a_ptr' 4
    b' <- load b_ptr' 4
    mul a' b'

  Core.MulOpI a b -> do
    a_ptr' <- genExp env a 
    b_ptr' <- genExp env b
    a' <- load a_ptr' 4
    b' <- load b_ptr' 4
    mul a' b'

genPtrOp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.PtrOp -> IRBuilderT m Operand
genPtrOp env = \case
  Core.RefOp e -> genExp env e  -- We already keep everything in pointers

  -- To deref, assume that e generates an operand which is
  -- a pointer to a pointer. We simply load the pointer,
  -- because this gives us a pointer to a value, which is
  -- basically the raw value in llvm.
  Core.DerefOp e -> do
    e_ptr_ptr' <- genExp env e 
    load e_ptr_ptr' 4


genMemOp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.MemOp -> IRBuilderT m Operand
genMemOp env = \case
  Core.ConstrOp (Core.Constr n args) ->
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (_, ty, con_ty, i, _) -> do
        ptr <- alloca ty Nothing 4
        
        tag <- gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
        store tag 4 (ConstantOperand $ C.Int 8 (toInteger i))

        con_ptr <- bitcast ptr (AST.ptr con_ty)
        mapM_ (genConstrArg env con_ptr) (zip args [1..])

        return ptr

  Core.NewOp (Core.Constr n args) -> mdo
    case envLookupConstr n env of
      Nothing -> error $ "undefined constructor: " ++ n
      Just (ty_n, ty, con_ty, i, _) ->
        case envLookupSize ty_n env of
          Nothing -> error $ "unsized constructor: " ++ n
          Just s -> do
            ptr_i8 <- genExp env (Core.ECall (Core.EVal $ Core.VVar "malloc") [Core.EVal $ Core.VInt (s+1)])
            ptr <- bitcast ptr_i8 (AST.ptr $ AST.ptr ty)

            ptr' <- load ptr 4
            tag <- gep ptr' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
            store tag 4 (ConstantOperand $ C.Int 8 (toInteger i))

            con_ptr <- bitcast ptr' (AST.ptr con_ty)
            mapM_ (genConstrArg env con_ptr) (zip args [1..])

            return ptr


  Core.FreeOp e ->
    genExp env (Core.ECall (Core.EVal $ Core.VVar "free") [e])


  Core.MemAccessI a i_val -> mdo
    i_ptr <- genExp env i_val
    i <- load i_ptr 4
    a' <- genExp env a
    gep a' [ConstantOperand $ C.Int 32 0, i]

  Core.MemAccess a n -> mdo
    case envLookupMember n env of
      Nothing -> error $ "unrecognized member name: " ++ n
      Just (con_ty, i)  -> do
        a' <- genExp env a
        b <- bitcast a' (AST.ptr con_ty)
        gep b [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]

 
  Core.MemUpdate n rhs -> mdo
    case envLookupLocal n env of
      Nothing -> error $ "unrecognized variable: " ++ n
      Just v  -> mdo
        rhs_ptr' <- genExp env rhs
        rhs' <- load rhs_ptr' 4
        store v 4 rhs'
        return v


genArrayOp :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.MemOp -> IRBuilderT m Operand
genArrayOp env = undefined

genConstrArg :: (MonadFix m, MonadModuleBuilder m) => Env -> Operand -> (Core.Exp, Int) -> IRBuilderT m ()
genConstrArg env ptr (arg, i) = do
  ptr' <- gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]
  arg' <- genExp env arg
  op <- load arg' 4
  store ptr' 4 op



genValue :: (MonadFix m, MonadModuleBuilder m) => Env -> Core.Val -> IRBuilderT m Operand
genValue env = \case
  Core.VInt i -> do
    ptr <- alloca i32 Nothing 4
    store ptr 4 (ConstantOperand $ C.Int 32 (toInteger i))
    return ptr

  Core.VString str -> do
    str_gptr <- globalStringPtr str =<< freshName "gstring"
    str_gptr' <- bitcast str_gptr (AST.ptr i8)
    val_ptr <- alloca (AST.ptr i8) Nothing 4
    store val_ptr 4 str_gptr
    return val_ptr

  Core.VVar n -> case envLookupLocal n env of
    Nothing -> error $ "undefined var: " ++ n
    Just local -> return local



-- Helpers for conversion from strings to names

str2Name :: String -> Name
str2Name = Name . str2sbs

str2Param :: String -> ParameterName
str2Param = ParameterName . str2sbs

str2sbs :: String -> ShortByteString
str2sbs = SBS.toShort . S8.pack