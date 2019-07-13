{

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.STLC.Parse where

import Language.STLC.Lex
import Language.STLC.Lex.Token
import Language.STLC.Syntax
import Language.Syntax.Location

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.DList (DList)
import qualified Data.DList as DL

import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc

}

%name parseModule module_doc
%tokentype { Token }
-- %lexer { lexNonSpace >>= } { Token TokenEof _ _ }
-- %monad { P } { >>= } { return }
%errorhandlertype explist
%error { expParseError }


%token
  '\\'               { Token (TokenRsvp "\\") _ $$ }
  '->'               { Token (TokenRsvp "->") _ $$ }
  '|'                { Token (TokenRsvp "|") _ $$ }
  ':'                { Token (TokenRsvp ":") _ $$ }
  ':='               { Token (TokenRsvp ":=") _ $$ }
  ','                { Token (TokenRsvp ",") _ $$ }
  '.'                { Token (TokenRsvp ".") _ $$ }
  '='                { Token (TokenRsvp "=") _ $$ }
  '_'                { Token (TokenRsvp "_") _ $$ }
  '+'                { Token (TokenRsvp "+") _ $$ }
  '-'                { Token (TokenRsvp "-") _ $$ }
  '*'                { Token (TokenRsvp "*") _ $$ }
  '&'                { Token (TokenRsvp "&") _ $$ }

  '<['               { Token (TokenRsvp "]>") _ $$ }
  ']>'               { Token (TokenRsvp "<[") _ $$ }

  '{'                { Token (TokenRsvp "{") _ $$ }
  '}'                { Token (TokenRsvp "}") _ $$ }
  '('                { Token (TokenRsvp "(") _ $$ }
  ')'                { Token (TokenRsvp ")") _ $$ }
  '['                { Token (TokenRsvp "[") _ $$ }
  ']'                { Token (TokenRsvp "]") _ $$ }

  'I1'               { Token (TokenRsvp  "I1" ) _ $$ }
  'I8'               { Token (TokenRsvp  "I8" ) _ $$ }
  'I16'              { Token (TokenRsvp "I16" ) _ $$ }
  'I32'              { Token (TokenRsvp "I32" ) _ $$ }
  'I64'              { Token (TokenRsvp "I64" ) _ $$ }

  'U8'               { Token (TokenRsvp  "U8" ) _ $$ }
  'U16'              { Token (TokenRsvp "U16" ) _ $$ }
  'U32'              { Token (TokenRsvp "U32" ) _ $$ }
  'U64'              { Token (TokenRsvp "U64" ) _ $$ }

  'F16'              { Token (TokenRsvp  "F16" ) _ $$ }
  'F32'              { Token (TokenRsvp  "F32" ) _ $$ }
  'F64'              { Token (TokenRsvp  "F64" ) _ $$ }
  'F128'             { Token (TokenRsvp "F128" ) _ $$ }

  'Array'            { Token (TokenRsvp  "Array" ) _ $$ }
  'Vect'             { Token (TokenRsvp   "Vect" ) _ $$ }

  'let'              { Token (TokenRsvp "let"   ) _ $$ }
  'in'               { Token (TokenRsvp "in"   ) _ $$ }
  'as'               { Token (TokenRsvp "as"   ) _ $$ }
  'case'             { Token (TokenRsvp "case"   ) _ $$ }
  'of'               { Token (TokenRsvp "of"   ) _ $$ }

  'if'               { Token (TokenRsvp "if"   ) _ $$ }
  'then'             { Token (TokenRsvp "then"   ) _ $$ }
  'else'             { Token (TokenRsvp "else"   ) _ $$ }
  'elif'             { Token (TokenRsvp "elif"   ) _ $$ }

  'new'              { Token (TokenRsvp "new"  ) _ $$ }
  'resize'           { Token (TokenRsvp "resize") _ $$ }
  'delete'           { Token (TokenRsvp "delete") _ $$ }

  'module'           { Token (TokenRsvp "module") _ $$ }
  'import'           { Token (TokenRsvp "import") _ $$ }

  'type'             { Token (TokenRsvp "type" ) _ $$ }
  'extern'           { Token (TokenRsvp "extern" ) _ $$ }

  varId              { Token (TokenVarId  _) _ _ }
  conId              { Token (TokenConId  _) _ _ }

  '#add'             { Token (TokenPrimId "#add") _ $$ }
  '#sub'             { Token (TokenPrimId "#sub") _ $$ }
  '#mul'             { Token (TokenPrimId "#mul") _ $$ }
  '#div'             { Token (TokenPrimId "#div") _ $$ }
  '#rem'             { Token (TokenPrimId "#rem") _ $$ }
  '#neg'             { Token (TokenPrimId "#nef") _ $$ }

  '#and'             { Token (TokenPrimId "#and") _ $$ }
  '#or'              { Token (TokenPrimId  "#or") _ $$ }
  '#xor'             { Token (TokenPrimId "#xor") _ $$ }
  '#shr'             { Token (TokenPrimId "#shr") _ $$ }
  '#shl'             { Token (TokenPrimId "#shl") _ $$ }

  '#eq'              { Token (TokenPrimId "#eq") _ $$ }
  '#neq'             { Token (TokenPrimId "#neq") _ $$ }
  '#lt'              { Token (TokenPrimId "#lt") _ $$ }
  '#le'              { Token (TokenPrimId "#le") _ $$ }
  '#gt'              { Token (TokenPrimId "#gt") _ $$ }
  '#ge'              { Token (TokenPrimId "#le") _ $$ }


  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }
  'null'             { Token (TokenRsvp "null") _ $$ }

  begin_line         { Token TokenLn _ _ }
  end_line           { Token TokenLn' _ _ }

  begin_block        { Token TokenBlk _ _ }
  end_block          { Token TokenBlk' _ _ }

  EOF                { Token TokenEof _ _ }

%%

-- Associativity

-- -----------------------------------------------------------------------------
-- | Helpers

some(p) :: { [_] }
  : some_dl(p)        { DL.toList $1 }

some_ne(p) :: { NonEmpty _ }
  : some(p)         { NE.fromList $1 }

some_dl(p) :: { DList _ }
  : some_dl(p) p   { DL.snoc $1 $2 }
  | p              { DL.singleton $1 }

-- | Zero or more occurences of 'p'
many(p) :: { [_] }
  : some(p)     { $1 }
  | {- empty -} { [] }


sep_by1(p,sep) :: { [_] }
  : sep_by1_dl(p,sep) { DL.toList $1 }

sep_by1_ne(p,sep) :: { NonEmpty _ }
  : sep_by1(p,sep) { NE.fromList $1 }

sep_by1_dl(p,sep) :: { DList _ }
  : sep_by1_dl(p,sep) sep p  { DL.snoc $1 $3 }
  | p                        { DL.singleton $1 }

linefold(p) :: { _ }
  : begin_line p end_line { $2 }

block(p) :: { [_] }
  : begin_block many(linefold(p)) end_block { $2 }

block1(p) :: { NonEmpty _ }
  : begin_block some_ne(linefold(p)) end_block { $2 }


-- -----------------------------------------------------------------------------
-- | Names and Values

mod_id :: { L String }
  : con_id { $1 }

var_id :: { L String }
  : varId { fmap unpack (extractId $1) }

con_id :: { L String }
  : conId { fmap unpack (extractId $1) }

literal :: { L Lit }
  : 'null'   { L LNull $1 }
  | integer  { fmap (LInt . fromInteger) (extractInteger $1) }
  | double   { fmap LDouble (extractDouble $1) }
  | char     { fmap LChar   (extractChar   $1) }
  | boolean  { fmap LBool   (extractBool   $1) }
  | string   { fmap LString (extractString $1) }


-- -----------------------------------------------------------------------------
-- | Definitions

module_doc :: { Module }
  : linefold(module_defn) many(linefold(defn)) mayeof { Module (locOf $1) (unL $1) $2 }

mayeof :: { Maybe Token }
  : {- Empty -} { Nothing }
  | EOF         { Just $1 }

module_defn :: { L String }
  : 'module' con_id { L "" $1 <> $2 }  

defn :: { Defn }
  : func_defn_typed   { FuncDefn $1 }
  | extern_defn       { ExternDefn $1 }
  | datatype_defn     { DataTypeDefn $1 }


-- -----------------------------------------------------------------------------
-- | Function

func_defn_typed :: { Func }
  : func_sig end_line begin_line func_defn
    { let ((L n1 l1, ty), (n2, ps, body)) = ($1, $4)
      in if n1 == n2
          then func (l1 <> locOf body) n1 ty ps body
          else error "Cannot parse function definition! Type and function name mismatch."  
    }

func_sig :: { (L String, Type) }
  : var_id ':' type { ($1, $3) }
  | error { undefined } -- expects function signature!!

func_defn :: { (String, [Pat], Exp) }
  : var_id apats0 '=' exp { (unL $1, $2, $4) }


-- -----------------------------------------------------------------------------
-- | Patterns

pat :: { Pat }
  : cpat { $1 }

cpat :: { Pat }
  : bpat ':' type { PLoc (PType $1 $3) ($1 <++> $3) }
  | bpat { $1 }

bpat :: { Pat }
  : con_id apats { PLoc (PCon (unL $1) $2) ($1 <++> $2) }
  | apat { $1 }

apat :: { Pat }
  : var_id      { PLoc (pvar (unL $1)) (locOf $1)  }
  | con_id      { PLoc (PCon (unL $1) []) (locOf $1) }
  | '_'         { PLoc PWild (locOf $1) }
  | '(' pat ')' { PLoc (PParens $2) ($1<>$3) }
  | '(' pat ',' some_ne(pat) ')' { PLoc (PTuple $2 $4) ($1<>$5) }

apats0 :: { [Pat] }
  : {- Empty -} { [] }
  | apats       { $1 }

apats :: { [Pat] }
  : apats_r      { reverse $1 }

apats_r :: { [Pat] }
  : apat { [$1] }
  | apats_r apat { $2:$1 }

-- -----------------------------------------------------------------------------
-- | Extern

extern_defn :: { Extern }
  : 'extern' var_id ':' type { let (paramtys, retty) = splitType $4
                               in Extern (locOf $2) (unL $2) paramtys retty 
                             } 

-- -----------------------------------------------------------------------------
-- | DataType

datatype_defn :: { DataType }
  : 'type' con_id             { DataType ($1 <++> $2) (unL $2) [] }
  | 'type' con_id '=' constrs { DataType ($1 <++> $4) (unL $2) $4 }

constrs :: { [ConstrDefn] }
  : sep_by1(constr_defn, '|') { $1 }

constr_defn :: { ConstrDefn }
  : con_id many(atype)
    { ConstrDefn ($1 <++> $2) (unL $1) $2 }
  
  | con_id '{' sep_by1_ne(entry_defn, ',') '}'
    { RecordDefn ($1 <++> $4) (unL $1) $3 }

entry_defn :: { Entry }
  : var_id ':' type { Entry ($1 <++> $3) (unL $1) $3 }


-- -----------------------------------------------------------------------------
-- | Expressions

exp :: { Exp }
  : dexp { $1 }

dexp :: { Exp }
  : cexp ':' type  { ELoc (EType $1 $3) ($1 <++> $3) }
  | cexp 'as' type { ELoc (ECast $1 $3) ($1 <++> $3) }
  | cexp { $1 }

cexp :: { Exp }
  : 'let' block(equation) 'in' cexp  
                                { ELoc (elet $2 $4) ($1 <++> $4) }
  | 'if' bexp 'then' exp elsebr { ELoc (EIf $2 $4 $5) ($1 <++> $5) }
  | 'case' bexp 'of' clauses    { ELoc (ECase $2 $4) ($1 <++> $4) }
  | '\\' apats '->' exp         { ELoc (elam $2 $4) ($1 <++> $4) }
  | bexp ':=' cexp              { ELoc (ESet $1 $3) ($1 <++> $3) }
  | bexp { $1 }

equation :: { (Pat, Exp) }
  : pat '=' exp { ($1, $3) }

elsebr :: { Else }
  : 'else' exp { Else (Just $ $1 <++> $2) $2 }
  | 'elif' bexp 'then' exp elsebr { Elif (Just $ $1 <++> $5) $2 $4 $5 }

clauses :: { NonEmpty Clause }
  : block1(clause) { $1 }

clause :: { Clause }
  : pat '->' exp { clause $1 $3 }

bexp :: { Exp }
  : aexp some_ne(aexp)        { ELoc (EApp $1 $2) ($1 <++> $2)  }
  | con_id                    { ELoc (ECon (unL $1) []) (locOf $1) }
  | con_id some(aexp)         { ELoc (ECon (unL $1) $2) ($1 <++> $2) }
  | 'new' con_id              { ELoc (ENewCon (unL $2) []) (locOf $1) }
  | 'new' con_id some(aexp)   { ELoc (ENewCon (unL $2) $3) ($1 <++> $2) }

  | 'new' 'Array' '[' exp ']' 
                              { ELoc (ENewArrayI $4) ($1 <++> $5) }
  | 'new' '[' sep_by1(exp, ',') ']'
                              { ELoc (ENewArray $3) ($1<>$4) }

  | 'new' 'Vect' '<[' exp ']>'
                              { ELoc (ENewVectI $4) ($1 <++> $5) }
  | 'new' '<[' sep_by1(exp, ',') ']>'
                              { ELoc (ENewVect $3) ($1<>$4) }

  | 'delete' aexp             { ELoc (EFree $2) ($1 <++> $2) }
  | aexp { $1 }


aexp :: { Exp }
  : var_id            { ELoc (evar (unL $1)) (locOf $1) }
  | literal           { ELoc (ELit (unL $1)) (locOf $1) }
  | primOp            { ELoc (EOp (unL $1)) (locOf $1) }
  | '&' aexp          { ELoc (ERef $2) ($1<++>$2) }
  | '*' aexp          { ELoc (EDeref $2) ($1<++>$2) }
  | aexp '.' var_id   { ELoc (EGet $1 (unL $3)) ($1<++>$3) }
  
  | aexp '[' exp ']'  { ELoc (EGetI $1 $3) ($1<++>$4) }
  
  | '[' sep_by1(exp, ',') ']' 
                      { ELoc (ELit (LArray $2)) ($1<>$3) }
  | 'Array' '[' aexp ']'  
                      { ELoc (ELit (LArrayI $3)) ($1<++>$4) }

  | '<[' sep_by1(exp, ',') ']>'
                      { ELoc (ELit (LVect $2)) ($1<>$3) }
  | 'Vect' '<[' aexp ']>' { ELoc (ELit (LVectI $3)) ($1<++>$4) }
  
  | '(' exp ',' sep_by1_ne(exp, ',') ')'
                      { ELoc (ETuple $2 $4) ($1<>$5) }
  | '(' exp ')'       { ELoc $2 ($1<>$3) }


primOp :: { L Op }
  : '#add' aexp aexp { L (OpAdd $2 $3) ($1<++>$3) }
  | '#sub' aexp aexp { L (OpSub $2 $3) ($1<++>$3) }
  | '#mul' aexp aexp { L (OpMul $2 $3) ($1<++>$3) }
  | '#div' aexp aexp { L (OpDiv $2 $3) ($1<++>$3) }
  | '#rem' aexp aexp { L (OpRem $2 $3) ($1<++>$3) }
  | '#neg' aexp      { L (OpNeg $2)     ($1<++>$2) }

  | '#and' aexp aexp { L (OpAnd $2 $3) ($1<++>$3) }
  | '#or'  aexp aexp { L (OpOr $2 $3)  ($1<++>$3) }
  | '#xor' aexp aexp { L (OpXor $2 $3) ($1<++>$3) }
  | '#shr' aexp aexp { L (OpShr $2 $3) ($1<++>$3) }
  | '#shl' aexp aexp { L (OpShL $2 $3) ($1<++>$3) }


  | '#eq'  aexp aexp { L (OpEqI $2 $3)  ($1<++>$3) }
  | '#neq' aexp aexp { L (OpNeqI $2 $3) ($1<++>$3) }

  | '#lt'  aexp aexp { L (OpLT $2 $3) ($1<++>$3) }
  | '#le'  aexp aexp { L (OpLE $2 $3) ($1<++>$3) }
  | '#gt'  aexp aexp { L (OpGT $2 $3) ($1<++>$3) }
  | '#ge'  aexp aexp { L (OpGE $2 $3) ($1<++>$3) }

-- -----------------------------------------------------------------------------
-- | Types

type :: { Type } 
  : btype { $1 }

btype :: { Type }
  : atype '->' btype { TLoc (TArr $1 $3) ($1<++>$3) }
  | atype { $1 }

atype :: { Type }
  : con_id       { TLoc (TCon $ unL $1) (locOf $1) }
  | primType     { $1 }
  | '*' atype    { TLoc (TPtr $2) ($1 <++> $2) }
  | '[' integer type ']' 
                { TLoc (TArray (fromInteger $ unL $ extractInteger $2) $3)
                       ($1<>$4)
                }
  | '(' type ')' { TLoc (TParens $2) ($1<>$3) }
  | '(' type ',' some_ne(type) ')' { TLoc (TTuple $2 $4) ($1<>$5) }


primType :: { Type }
  : 'I1'  { TLoc (TInt  1) (locOf $1) }
  | 'I8'  { TLoc (TInt  8) (locOf $1) }
  | 'I16' { TLoc (TInt 16) (locOf $1) }
  | 'I32' { TLoc (TInt 32) (locOf $1) }
  | 'I64' { TLoc (TInt 64) (locOf $1) }

  | 'U8'  { TLoc (TUInt  8) (locOf $1) }
  | 'U16' { TLoc (TUInt 16) (locOf $1) }
  | 'U32' { TLoc (TUInt 32) (locOf $1) }
  | 'U64' { TLoc (TUInt 64) (locOf $1) }

  | 'F16'  { TLoc (TFp  16) (locOf $1) }
  | 'F32'  { TLoc (TFp  32) (locOf $1) }
  | 'F64'  { TLoc (TFp  64) (locOf $1) }
  | 'F128' { TLoc (TFp 128) (locOf $1) }


{

parseError :: [Token] -> a
parseError [] = error "Parse error, no tokens left!!"
parseError (t:_) = error $ "Parse error on token: " ++ show (pretty t)

expParseError :: ([Token], [String]) -> a
expParseError ([], s)  = error $ "Parse error:"
                               ++ "Expected: " ++ show s
expParseError (ts, s) = error $ "Parse error on tokens: " ++ show (pretty $ head ts) ++ "\n"
                               ++ "Expected: " ++ show (punctuate "," $ pretty <$> s)

}
