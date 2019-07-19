{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings,
             LambdaCase,
             RecursiveDo,
             TupleSections,
             ViewPatterns,
             ConstraintKinds
  #-}
module Language.LLTT.LLVM.Codegen where

import Prelude hiding (log)

import Language.Syntax.Location
import qualified Language.LLTT.Syntax as LL
import Language.LLTT.Pretty

import Control.Monad
import Control.Monad.Fix

import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc as PP

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.List as L

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
import LLVM.AST.FloatingPointPredicate as Fp hiding (True, False)
import LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import qualified LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as I

-- import System.IO.Unsafe
-- import Debug.Trace

log :: String -> a -> a
log msg a = a -- unsafePerformIO (putStrLn msg) `seq` a

type MonadCodeGen m = (MonadFix m, MonadModuleBuilder m)

data Env = Env { envTypes     :: Map String Type
               , envFuncs     :: Map String Operand
               , envLocals    :: Map String Operand
               , envSizes     :: Map String Int
               , envTypeDefs  :: Map String Type
               , envConstrs   :: Map String (String, Type, Type, Int, [Type])
               , envMembers   :: Map String (Type, Int)
               , envLoc       :: Loc
               }

mkEnv :: Loc -> Env
mkEnv l = Env { envTypes = mempty
               , envFuncs = mempty
               , envLocals = mempty
               , envSizes = mempty 
               , envTypeDefs = mempty
               , envConstrs = mempty
               , envMembers = mempty
               , envLoc = l
               }

envGetLoc :: Env -> Loc
envGetLoc = envLoc 

envInsertTypes :: [(String, Type)] -> Env -> Env
envInsertTypes types env = foldl' go env types
  where go env (n, ty) = envInsertType n ty env 

envInsertType :: String -> Type -> Env -> Env
envInsertType n ty env = env { envTypes = Map.insert n ty (envTypes env) }

envLookupType :: String -> Env -> Maybe Type
envLookupType n env = Map.lookup n (envTypes env)

envSetLoc :: Loc -> Env -> Env
envSetLoc l env = env { envLoc = l }

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
    Nothing -> 
      error $ show $ vsep [ PP.line <> pretty (envLoc env) <+> "error:"
                          , indent 4 $ "envLookupFunc - Couldn't find function:" <+> pretty n
                          , PP.line
                          ]


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


genModule :: Env -> FilePath -> LL.Module -> Module
genModule env1 srcfp (LL.Module l n defns)
  = m { moduleSourceFileName = str2sbs srcfp } 
  where
    m = buildModule (str2sbs n) $ mdo
          env2 <- foldM genDecl (env1 {envLoc = l}) defns
          ops <- mapM (genFunc env3) [f | (LL.FuncDefn f) <- defns]
          let env3 = foldl' (\env (n, f, ty) ->
                                  envInsertFunc n f
                                $ envInsertType n ty env) env2 ops
          return ()

genFunc :: (MonadCodeGen m) => Env -> LL.Func -> m (String, Operand, Type)
genFunc env (LL.Func _ name args body) = do
  let paramtys = LL.exPType <$> args
      paramtys' = genType env <$> paramtys
      params = map (, NoParameterName) paramtys'
      retty = LL.exType body
      ty = genType env (LL.TFunc retty (NE.fromList paramtys))
  f' <- function (str2Name name) params (genType env retty) $ \args' -> do
    entry <- block `named` "entry"
    args'' <- mapM genFuncParam (zip paramtys' args')
    env' <-  genPatExtractMany env (zip args args'')
    genBody env' body
  return (name, f', ty)


genDecl :: (MonadCodeGen m) => Env -> LL.Defn -> m Env
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

  LL.ExternDefn ex@(LL.Extern _ n paramtys retty) -> do
    ex' <- genExtern env ex
    return $ envInsertFunc n ex'
           $ envInsertType n (genType env (LL.TFunc retty (NE.fromList paramtys)))
           $ env
  
  LL.DataTypeDefn dt@(LL.DataType _ n _) -> do
    let s = LL.sizeDataType (envSizes env) dt
    log ("Generating datatype " ++ n ++ " with size " ++ show s) $ return ()
    (ty_n, ty, cons) <- genDataType env s dt
    return . (\env -> foldl' (\env (_, con_ty, _, mems) -> envInsertMembers con_ty mems env) env cons)
           . envInsertSize ty_n s
           . envInsertTypeDef n ty
           . envInsertConstrs [(ty_n, ty, con_n, con_ty, ty_params) | (con_n, con_ty, ty_params, _) <- cons]
           $ env

  _ -> return env

genFuncParam :: (MonadCodeGen m) => (Type, Operand) -> IRBuilderT m Operand
genFuncParam (ty, op) = do
  ptr <- alloca ty Nothing 4
  store ptr 4 op
  return ptr

genVars :: (MonadCodeGen m) => Env -> [(String, Type)] -> IRBuilderT m Env
genVars = foldM genVar

genVar :: (MonadCodeGen m) => Env -> (String, Type) -> IRBuilderT m Env
genVar env (n, ty) = do
  ptr <- alloca ty Nothing 4
  return $ envInsertLocal n ptr
         $ envInsertType n ty
         $ env

genExtern :: (MonadCodeGen m) => Env -> LL.Extern -> m Operand
genExtern env (LL.Extern _ name argtys retty) =
  extern (str2Name name) (genType env <$> argtys) (genType env retty)

genDataType :: (MonadCodeGen m) => Env -> Int -> LL.DataType -> m (String, Type, [(String, Type, [Type], [Maybe String])])
genDataType env s dt@(LL.DataType _ n cons) = do
  ty <- typedef (str2Name n) (Just $ StructureType True [i8, ArrayType (fromIntegral s) i8] )
  let env' = envInsertTypeDef n ty env
  cons' <- mapM (genConstrDefn env' n) cons
  return (n, ty, cons')

genConstrDefn :: (MonadCodeGen m) => Env -> String -> LL.ConstrDefn -> m (String, Type, [Type], [Maybe String])
genConstrDefn env dt_name = \case
  LL.ConstrDefn _ con_n ty_params -> do
    let n = dt_name ++ "_" ++ con_n
        ty_params' = genType env <$> ty_params
    (con_n,,,) <$> typedef (str2Name n) (Just $ StructureType True (i8 : ty_params') )
               <*> pure ty_params'
               <*> pure []

  LL.RecordDefn _ con_n (NE.toList -> ens) -> do
    let n = dt_name ++ "_" ++ con_n
        ns = [n | LL.Entry l n ty <- ens]
        ty_params' = [genType env ty | LL.Entry l n ty <- ens]
    (con_n,,,) <$> typedef (str2Name n) (Just $ StructureType True (i8 : ty_params') )
               <*> pure ty_params'
               <*> pure (Just <$> ns)

genType :: Env -> LL.Type -> Type
genType env = \case
  LL.TVar n -> error "LLTT doesn't support type variables yet!"
  LL.TCon n ->
      case envLookupTypeDef n env of
        Nothing -> error $ "undefined type encountered: " ++ n
        Just ty -> ty
  LL.TInt 1  -> i1
  LL.TInt 8  -> i8
  LL.TInt 32 -> i32
  LL.TInt 64 -> i64

  LL.TUInt 1  -> i1
  LL.TUInt 8  -> i8
  LL.TUInt 32 -> i32
  LL.TUInt 64 -> i64

  LL.TFp 16 -> half
  LL.TFp 32 -> float
  LL.TFp 64 -> double
  LL.TFp 128 -> fp128

  LL.TTuple t (NE.toList -> ts) ->
    StructureType True (genType env <$> (t:ts))
  LL.TArray i ty -> ArrayType (fromIntegral i) (genType env ty)
  LL.TPtr t -> Ty.ptr (genType env t)
  LL.TFunc retty paramtys -> Ty.FunctionType (genType env retty)
                                             (genType env <$> NE.toList paramtys)
                                             False

  LL.TLoc t _ -> genType env t
  LL.TParens t -> genType env t

genBody :: (MonadCodeGen m) => Env -> LL.Exp -> IRBuilderT m ()
genBody env body = do
  body' <- genExp env body
  r <- load body' 4
  ret r

loadExp :: (MonadCodeGen m) => Env -> LL.Exp -> IRBuilderT m Operand
loadExp env e = do
  e_ptr <- genExp env e
  load e_ptr 4


genExp :: (MonadCodeGen m) => Env -> LL.Exp -> IRBuilderT m Operand
genExp env e = genExp' env (LL.exType e) e

genExp' :: (MonadCodeGen m) => Env -> LL.Type -> LL.Exp -> IRBuilderT m Operand
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

  LL.ECall f@((LL.exTyAnn . LL.exType) -> (LL.TFunc retty paramtys)) xs -> mdo
    br call_init
    
    call_init <- block `named` "call.init"
    r_ptr <- alloca (genType env retty) Nothing 4
    br call_gen_func

    call_gen_func <- block `named` "call.gen.func"
    f' <- genExp env f
    br call_gen_args

    call_gen_args <- block `named` "call.gen.args"
    xs' <- mapM (genExp env) (NE.toList xs)
    br call_load_args

    call_load_args <- block `named` "call.load.args"
    xs'' <- mapM (`load` 4) xs'
    br call_app

    call_app <- block `named` "call.app"
    r <- call f' $(,[]) <$> xs''
    store r_ptr 4 r
    return r_ptr

  LL.ECall f xs -> error $ "genExp - Expected a typed function.\n\n"
                       ++ show f ++ " " ++ show xs ++ "\n\n"

  LL.EType e rty' -> genExp' env rty' e

  LL.ECast e ty -> genCast env e ty

  LL.ELoc e l -> genExp' env{envLoc = l} rty e
  LL.EParens e -> genExp' env rty e

  LL.ELet qs body -> mdo
    br let_header
    let_header <- block `named` "let.header"
    
    let ps = fst <$> NE.toList qs
    env' <- foldM genPatAlloc env ps

    let go (p, e) = mdo
          br rhs_blk

          rhs_blk <- block `named` "let.header.rhs"
          op <- genExp env' e
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


  LL.ETuple e (NE.toList -> es) -> mdo
    br tuple_init_blk

    tuple_init_blk <- block `named` "tuple.init"
    ty_ptr <- alloca (genType env rty) Nothing 4
    br tuple_gen_blk

    tuple_gen_blk <- block `named` "tuple.gen"
    e_ptrs' <- mapM (genExp env) (e:es)
    br tuple_load_blk
    
    tuple_load_blk <- block `named` "tuple.load"
    es' <- mapM (`load` 4) e_ptrs'
    br tuple_store_blk

    tuple_store_blk <- block `named` "tuple.store"
    let go (e', i) = do
          e_ptr' <- gep ty_ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
          store e_ptr' 4 e'
    mapM_ go (zip es' [0..])

    return ty_ptr

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
            c_ptr_ptr <- bitcast i8_ptr_ptr (Ty.ptr $ Ty.ptr ty)
            c_ptr <- load c_ptr_ptr 4
            tag_ptr <- gep c_ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 0]
            store tag_ptr 4 (ConstantOperand $ C.Int 8 (toInteger i))

            con_ptr <- bitcast c_ptr (Ty.ptr con_ty)
            mapM_ (genConstrArg env con_ptr) (zip args' [1..])

            return c_ptr_ptr

  LL.EFree e -> do
    let f = envLookupFunc "free" env
    e' <- loadExp env e
    p' <- bitcast e' (Ty.ptr Ty.i8)
    call f [(p', [])]

  LL.EGet e n -> do
    case envLookupMember n env of
      Nothing -> error $ "unrecognized member name: " ++ n
      Just (con_ty, i)  -> do
        e_ptr' <- genExp env e
        log "EGet accessing gep" $ gep e_ptr' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]

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
    forM_ (zip xs' [0..]) $ \(x, i) -> do
      e_ptr <- gep arr_ptr [ConstantOperand $ C.Int 32 i]
      store e_ptr 4 x

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

  LL.ENewString str -> mdo
    -- br newstr_stage_blk
    -- newstr_stage_blk <- block `named` "newstr.stage"
    -- str_ptr <- globalStringPtr str =<< freshUnName
    br newstr_alloc_handle_blk

    newstr_alloc_handle_blk <- block `named` "newstr.alloc.handle"
    i8_ptr_ptr <- alloca (Ty.ptr Ty.i8) Nothing 4
    br newstr_alloc_content_blk

    newstr_alloc_content_blk <- block `named` "newstr.alloc.content"
    let f = envLookupFunc "malloc" env
    n <- C.int32 $ toInteger $ length $ str
    r <- call f [(n, [])]
    store i8_ptr_ptr 4 r
    br newstr_store_blk

    newstr_store_blk <- block `named` "newstr.store"
    forM_ (zip str [0..]) $ \(c, i) -> do
      x <- gep r [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
      store x 4 =<< C.int8 (fromIntegral $ ord c)

    return i8_ptr_ptr


  LL.EOp op ->
    genOp env rty op


genConstrArg :: (MonadCodeGen m) => Env -> Operand -> (Operand, Int) -> IRBuilderT m ()
genConstrArg env ptr (arg_ptr, i) = do
  eptr <- gep ptr [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (fromIntegral i)]
  arg <- load arg_ptr 4
  store eptr 4 arg


genLit :: (MonadCodeGen m) => Env -> LL.Type -> LL.Lit -> IRBuilderT m Operand
genLit env ty = \case
  LL.LInt i ->
    case (LL.exTyAnn ty) of
      LL.TInt 1 -> do
        ptr <- alloca i8 Nothing 4
        i' <- C.int8 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TInt 8 -> do
        ptr <- alloca i8 Nothing 4
        i' <- C.int8 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TInt 16 -> do
        ptr <- alloca i8 Nothing 4
        i' <- C.int8 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TInt 32 -> do
        ptr <- alloca i32 Nothing 4
        i' <- C.int32 $ toInteger i
        store ptr 4 i'
        return ptr
      
      LL.TInt 64 -> do
        ptr <- alloca i64 Nothing 4
        i' <- C.int64 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TUInt 8 -> do
        ptr <- alloca i8 Nothing 4
        i' <- C.int8 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TUInt 16 -> do
        ptr <- alloca i8 Nothing 4
        i' <- C.int8 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TUInt 32 -> do
        ptr <- alloca i32 Nothing 4
        i' <- C.int32 $ toInteger i
        store ptr 4 i'
        return ptr
      
      LL.TUInt 64 -> do
        ptr <- alloca i64 Nothing 4
        i' <- C.int64 $ toInteger i
        store ptr 4 i'
        return ptr

      LL.TFp 16 -> do
        ptr <- alloca Ty.float Nothing 4
        d' <- C.single $ fromIntegral i
        store ptr 4 d'
        return ptr

      LL.TFp 32 -> do
        ptr <- alloca Ty.float Nothing 4
        d' <- C.single $ fromIntegral i
        store ptr 4 d'
        return ptr
      
      LL.TFp 64 -> do
        ptr <- alloca Ty.double Nothing 4
        d' <- C.double $ fromIntegral i
        store ptr 4 d'
        return ptr

      LL.TFp 128 -> do
        ptr <- alloca Ty.double Nothing 4
        d' <- C.double $ fromIntegral i
        store ptr 4 d'
        return ptr

      _ -> error $ "Expected integer or double type, found: " ++ show ty

  LL.LDouble d ->
    case LL.exTyAnn ty of
      -- LL.TFp 16 -> do
      --   ptr <- alloca Ty.half Nothing 4
      --   d' <- C.half $  d
      --   store ptr 4 d'
      --   return ptr

      LL.TFp 32 -> do
        ptr <- alloca Ty.float Nothing 4
        d' <- C.single $ realToFrac d
        store ptr 4 d'
        return ptr
      
      LL.TFp 64 -> do
        ptr <- alloca Ty.double Nothing 4
        d' <- C.double d
        store ptr 4 d'
        return ptr

      -- LL.TFp 128 -> do
      --   ptr <- alloca Ty.fp128 Nothing 4
      --   d' <- C.single $ realToFrac d
      --   store ptr 4 (ConstantOperand $ C.Float (F.Quadruple $ d d))
      --   return ptr

      _ -> error $ "Expected double type, found: " ++ show ty


  LL.LBool b -> do
    ptr <- alloca i1 Nothing 4
    b' <- C.bit $ toInteger $ fromEnum b
    store ptr 4 b'
    return ptr

  LL.LChar c -> do
    ptr <- alloca i8 Nothing 4
    c' <- C.int8 $ toInteger $ ord c
    store ptr 4 c'
    return ptr

  LL.LString str -> do
    str_ptr <- globalStringPtr str =<< freshUnName
    str_ptr' <- bitcast str_ptr (Ty.ptr i8)
    str_ptr_ptr <- alloca (Ty.ptr i8) Nothing 4
    store str_ptr_ptr 4 str_ptr
    return str_ptr_ptr
  
  LL.LArray xs -> mdo
    br arr_staging_blk

    arr_staging_blk <- block `named` "arr.staging"
    x_ptrs' <- mapM (genExp env) xs
    br arr_alloc_blk

    arr_alloc_blk <- block `named` "arr.alloc"
    let ety = genType env $ LL.exType $ head xs
        n = fromIntegral $ length xs
        arrty = Ty.ArrayType n ety
    arrptr <- alloca arrty Nothing 4
    arrptr' <- bitcast arrptr (Ty.ptr ety)
    arrptr_ptr <- alloca (Ty.ptr ety) Nothing 4
    br arr_init_blk

    arr_init_blk <- block `named` "arr.init"
    forM_ (zip x_ptrs' [0..]) $ \(x_ptr', i) -> do
      e_ptr <- gep arrptr' [ConstantOperand $ C.Int 32 i]
      x' <- load x_ptr' 4
      store e_ptr 4 x'

    store arrptr_ptr 4 arrptr'
    return arrptr_ptr

  LL.LGetI e i -> do
    r_ptr <- alloca (genType env ty) Nothing 4
    e_ptr' <- loadExp env e
    r <- gep e_ptr' [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 (toInteger i)]
    store r_ptr 4 r
    return r_ptr

  l -> error $ "genLit - undefined case encountered: " ++ show l

genElse :: (MonadCodeGen m) => Env -> LL.Else -> IRBuilderT m Operand
genElse env = \case
  LL.Else _ e -> genExp env e
  LL.Elif _ p t@(LL.exType -> ty) f ->
   genExp env (LL.EType (LL.EIf p t f) ty)

genOp :: (MonadCodeGen m) => Env -> LL.Type -> LL.Op -> IRBuilderT m Operand
genOp env rty = \case
  LL.OpAdd a b
    | LL.isIntTy rty || LL.isUIntTy rty
        -> genBinaryOp env add rty a b
    | LL.isFloatTy rty -> genBinaryOp env fadd rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpSub a b
    | LL.isIntTy rty || LL.isUIntTy rty
        -> genBinaryOp env sub rty a b
    | LL.isFloatTy rty -> genBinaryOp env fsub rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpMul a b
    | LL.isIntTy rty || LL.isUIntTy rty
        -> genBinaryOp env mul rty a b
    | LL.isFloatTy rty -> genBinaryOp env fmul rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpDiv a b
    | LL.isIntTy   rty -> genBinaryOp env sdiv rty a b
    | LL.isUIntTy  rty -> genBinaryOp env udiv rty a b
    | LL.isFloatTy rty -> genBinaryOp env fdiv rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpRem a b
    | LL.isIntTy  rty -> genBinaryOp env srem rty a b
    | LL.isUIntTy rty -> genBinaryOp env urem rty a b
    | LL.isFloatTy  rty -> genBinaryOp env frem rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpNeg a -> error "#neg not supported"

  LL.OpAnd a b -> genBinaryOp env I.and rty a b
  LL.OpOr  a b -> genBinaryOp env I.or  rty a b
  LL.OpXor a b -> genBinaryOp env I.xor rty a b

  LL.OpShR a b -> genBinaryOp env I.lshr rty a b
  LL.OpShL a b -> genBinaryOp env I.shl rty a b

  LL.OpEq a b
    | LL.isIntTy  rty || LL.isUIntTy rty
        -> genBinaryOp env (icmp AST.EQ) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.OEQ) rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpNeq a b
    | LL.isIntTy rty || LL.isUIntTy rty
        -> genBinaryOp env (icmp AST.NE) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.ONE) rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpLT a b
    | LL.isIntTy   rty -> genBinaryOp env (icmp AST.SLT) rty a b
    | LL.isUIntTy  rty -> genBinaryOp env (icmp AST.ULT) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.OLT) rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpLE a b
    | LL.isIntTy   rty -> genBinaryOp env (icmp AST.SLE) rty a b
    | LL.isUIntTy  rty -> genBinaryOp env (icmp AST.ULE) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.OLE) rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpGT a b
    | LL.isIntTy   rty -> genBinaryOp env (icmp AST.SGT) rty a b
    | LL.isUIntTy  rty -> genBinaryOp env (icmp AST.UGT) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.OGT) rty a b
    | otherwise -> error "Couldn't generate operation"

  LL.OpGE a b
    | LL.isIntTy   rty -> genBinaryOp env (icmp AST.SGE) rty a b
    | LL.isUIntTy  rty -> genBinaryOp env (icmp AST.UGE) rty a b
    | LL.isFloatTy rty -> genBinaryOp env (fcmp Fp.OGE) rty a b
    | otherwise -> error "Couldn't generate operation"


genBinaryOp :: (MonadFix m, MonadModuleBuilder m)
            => Env -> (Operand -> Operand -> IRBuilderT m Operand) -> LL.Type -> LL.Exp -> LL.Exp -> IRBuilderT m Operand
genBinaryOp env instr retty a b = do
    a' <- loadExp env a
    b' <- loadExp env b
    
    ptr <- alloca (genType env retty) Nothing 4
    op <- instr a' b'
    store ptr 4 op
    return ptr


-----------------------------------------------------------------------
-- Casting
-----------------------------------------------------------------------

genCast :: MonadCodeGen m => Env -> LL.Exp -> LL.Type -> IRBuilderT m Operand
genCast env e t2 = do
  r_ptr <- alloca (genType env t2) Nothing 4 
  e_ptr' <- genExp env e
  let t1 = LL.exType e
  case (LL.exTyAnn t1, LL.exTyAnn t2) of
    -- Casting booleans

    -- Casting from Ints
    (LL.TInt s1, LL.TInt s2)
      | s1 < s2  -> genSignedUpcast   env e_ptr' r_ptr t2
      | s1 > s2  -> genSignedDowncast env e_ptr' r_ptr t2
      | s1 == s2 -> genForcedCast     env e_ptr' r_ptr t2

    (LL.TInt s1, LL.TUInt s2) -- need to remove sign?? unclear how this is done...
      | s1 < s2  -> undefined
      | s1 > s2  -> undefined
      | s1 == s2 -> undefined -- remove sign??

    (LL.TInt s1, LL.TFp s2)
      -> genSigned2Fp env e_ptr' r_ptr t2

    -- Casting from Unsigned Ints
    (LL.TUInt s1, LL.TInt s2)
      | s1 < s2  -> genUnsignedUpcast   env e_ptr' r_ptr t2
      | s1 > s2  -> genUnsignedDowncast env e_ptr' r_ptr t2
      | s1 == s2 -> genForcedCast       env e_ptr' r_ptr t2

    (LL.TUInt s1, LL.TUInt s2)
      | s1 < s2  -> genUnsignedUpcast   env e_ptr' r_ptr t2
      | s1 > s2  -> genUnsignedDowncast env e_ptr' r_ptr t2
      | s1 == s2 -> genForcedCast       env e_ptr' r_ptr t2

    (LL.TUInt s1, LL.TFp s2)
      -> genUnsigned2Fp env e_ptr' r_ptr t2

    -- Casting from Floats
    (LL.TFp s1, LL.TInt s2)
      -> genSigned2Fp env e_ptr' r_ptr t2

    (LL.TFp s1, LL.TUInt s2)
      -> genFp2Signed env e_ptr' r_ptr t2

    (LL.TFp s1, LL.TFp s2)
      | s1 < s2  -> genFpUpcast   env e_ptr' r_ptr t2
      | s1 > s2  -> genFpDowncast env e_ptr' r_ptr t2
      | s1 == s2 -> genForcedCast env e_ptr' r_ptr t2


    (t1', t2')
      | LL.isIntTy   t1' && LL.isPtrTy   t2' -> genInt2Ptr env e_ptr' r_ptr t2
      | LL.isPtrTy   t1' && LL.isIntTy   t2' -> genPtr2Int env e_ptr' r_ptr t2
      | LL.isFloatTy t1' && LL.isPtrTy   t2' -> genFp2Ptr  env e_ptr' r_ptr t2
      | LL.isPtrTy   t1' && LL.isFloatTy t2' -> genPtr2Fp  env e_ptr' r_ptr t2
      | LL.isPtrTy   t1' && LL.isPtrTy   t2' -> genForcedCast env e_ptr' r_ptr t2
      | otherwise -> error $ show $ vsep
                        [ PP.line <> pretty (locOf e <> locOf t2) <+> "error:"
                        , indent 4 $ vsep
                            [ "Unsupported cast."
                            , "From:" <+> pretty t1
                            , "To:" <+> pretty t2
                            , "in"
                            , indent 2 $ pretty e
                            ]
                        , PP.line
                        ]
  
  return r_ptr


genUnsigned :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genUnsigned env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- bitcast e (genType env ty)
  store r_ptr 4 r

-- Int to Int casts
genForcedCast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genForcedCast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- bitcast e (genType env ty)
  store r_ptr 4 r

-- Int to Int casts
genSignedDowncast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genSignedDowncast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- trunc e (genType env ty)
  store r_ptr 4 r

genSignedUpcast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genSignedUpcast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- sext e (genType env ty)
  store r_ptr 4 r

genUnsignedDowncast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genUnsignedDowncast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- trunc e (genType env ty)
  store r_ptr 4 r

genUnsignedUpcast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genUnsignedUpcast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- zext e (genType env ty)
  store r_ptr 4 r

-- Float to Float casts
genFpDowncast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genFpDowncast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- fptrunc e (genType env ty)
  store r_ptr 4 r

genFpUpcast :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genFpUpcast env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- fpext e (genType env ty)
  store r_ptr 4 r

-- Casts between Int and Float
genSigned2Fp :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genSigned2Fp env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- sitofp e (genType env ty)
  store r_ptr 4 r

genFp2Signed :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genFp2Signed env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- fptosi e (genType env ty)
  store r_ptr 4 r

genUnsigned2Fp :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genUnsigned2Fp env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- uitofp e (genType env ty)
  store r_ptr 4 r

genFp2Unsigned :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genFp2Unsigned env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- fptoui e (genType env ty)
  store r_ptr 4 r


-- Pointer casts
genInt2Ptr :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genInt2Ptr env e_ptr r_ptr ty = do
  e' <- load e_ptr 4
  r <- inttoptr e' (genType env ty)
  store r_ptr 4 r

genPtr2Int :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genPtr2Int env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r <- ptrtoint e (genType env ty)
  store r_ptr 4 r

genFp2Ptr :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genFp2Ptr env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r1 <- fptosi e (genType env (LL.TUInt 64))
  r2 <- inttoptr r1 (genType env ty)
  store r_ptr 4 r2

genPtr2Fp :: MonadCodeGen m => Env -> Operand -> Operand -> LL.Type -> IRBuilderT m ()
genPtr2Fp env e_ptr r_ptr ty = do
  e <- load e_ptr 4
  r1 <- ptrtoint e (genType env (LL.TUInt 64))
  r2 <- sitofp r1 (genType env ty)
  store r_ptr 4 r2



-----------------------------------------------------------------------
-- Pattern variable extraction
-----------------------------------------------------------------------

genPatAlloc :: (MonadCodeGen m) => Env -> LL.Pat -> IRBuilderT m Env
genPatAlloc env p = genVars env (second (genType env) <$> LL.patFreeTyped p)

genPatInit :: (MonadCodeGen m) => Env -> LL.Pat -> Operand -> IRBuilderT m ()
genPatInit env p op = genPatInit' env p (LL.exPType p) op

genPatInit' :: (MonadCodeGen m) => Env -> LL.Pat -> LL.Type -> Operand -> IRBuilderT m ()
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
              arg_op <- gep con_op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
              genPatInit env p arg_op
        mapM_ go (zip ps [1..])

  LL.PTuple p (NE.toList -> ps) -> do
    let go (p, i) = do
          arg_op <- gep op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
          genPatInit env p arg_op
    mapM_ go (zip (p:ps) [0..])

  LL.PWild -> return ()
  LL.PType p ty' -> genPatInit' env p ty' op
  LL.PLoc p _  -> genPatInit' env p ty op
  LL.PParens p -> genPatInit' env p ty op

genPatExtractMany :: (MonadCodeGen m) => Env -> [(LL.Pat, Operand)] -> IRBuilderT m Env
genPatExtractMany = foldM (\env (p, op) -> genPatExtract env p op)

genPatExtract :: (MonadCodeGen m) => Env -> LL.Pat -> Operand -> IRBuilderT m Env
genPatExtract env p@(LL.exPType -> pty) op = genPatExtract' env p pty op

genPatExtract' :: (MonadCodeGen m) => Env -> LL.Pat -> LL.Type -> Operand -> IRBuilderT m Env
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
        let go env (p, i) = do
              op' <- gep con_op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
              genPatExtract env p op'
        foldM go env (zip ps [1..])

  LL.PTuple p (NE.toList -> ps) -> do
    let go env (p, i) = do
          op' <- gep op [ConstantOperand $ C.Int 32 0, ConstantOperand $ C.Int 32 i]
          genPatExtract env p op'
    foldM go env (zip (p:ps) [0..])

  LL.PWild -> return env
  LL.PType p pty' -> genPatExtract' env p pty' op
  LL.PLoc p _ -> genPatExtract' env p pty op
  LL.PParens p -> genPatExtract' env p pty op


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