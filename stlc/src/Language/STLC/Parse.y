{

{-# LANGUAGE OverloadedStrings #-}
module Language.STLC.Parse where

import Language.STLC.Lex
import Language.STLC.Lex.Token
import Language.STLC.Syntax
import Language.Syntax.Location

import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc

}

%name parseModule module_doc
%tokentype { Token }
%error { parseError }


%token
  '\\'               { Token (TokenRsvp "\\") _ $$ }
  '->'               { Token (TokenRsvp "->") _ $$ }
  '|'                { Token (TokenRsvp "|") _ $$ }
  ':'                { Token (TokenRsvp ":") _ $$ }
  ','                { Token (TokenRsvp ",") _ $$ }
  '.'                { Token (TokenRsvp ".") _ $$ }
  '='                { Token (TokenRsvp "=") _ $$ }
  '_'                { Token (TokenRsvp "_") _ $$ }
  '+'                { Token (TokenRsvp "+") _ $$ }
  '-'                { Token (TokenRsvp "-") _ $$ }
  '*'                { Token (TokenRsvp "*") _ $$ }
  '&'                { Token (TokenRsvp "&") _ $$ }

  '{'                { Token (TokenRsvp "{") _ $$ }
  '}'                { Token (TokenRsvp "}") _ $$ }
  '('                { Token (TokenRsvp "(") _ $$ }
  ')'                { Token (TokenRsvp ")") _ $$ }
  '['                { Token (TokenRsvp "[") _ $$ }
  ']'                { Token (TokenRsvp "]") _ $$ }

  'I8'               { Token (TokenRsvp "I8" ) _ $$ }
  'I32'              { Token (TokenRsvp "I32" ) _ $$ }
  'I64'              { Token (TokenRsvp "I64" ) _ $$ }
  'F32'              { Token (TokenRsvp "F32" ) _ $$ }
  'F64'              { Token (TokenRsvp "F64" ) _ $$ }
  'Bool'             { Token (TokenRsvp "Bool" ) _ $$ }
  'Char'             { Token (TokenRsvp "Char" ) _ $$ }
  'Array'            { Token (TokenRsvp "Array" ) _ $$ }
  'String'           { Token (TokenRsvp "String" ) _ $$ }
  'Void'             { Token (TokenRsvp "Void") _ $$ }

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

  '#fadd'            { Token (TokenPrimId "#fadd") _ $$ }
  '#fsub'            { Token (TokenPrimId "#fsub") _ $$ }
  '#fmul'            { Token (TokenPrimId "#fmul") _ $$ }

  integer            { Token (TokenInteger _) _ _ }
  double             { Token (TokenDouble  _) _ _ }
  char               { Token (TokenChar    _) _ _ }
  string             { Token (TokenString  _) _ _ }
  boolean            { Token (TokenBool    _) _ _ }

  begin_line         { Token TokenLn _ _ }
  end_line           { Token TokenLn' _ _ }

  begin_block        { Token TokenBlk _ _ }
  end_block          { Token TokenBlk' _ _ }

  EOF                { Token TokenEof _ _ }

%%

-- -----------------------------------------------------------------------------
-- | Names and Values

var_name :: { String }
var_name : var_id { unpack (unL $1) }

con_name :: { String }
con_name : con_id { unpack (unL $1) }

mod_name :: { String }
mod_name : con_name { $1 }

var_id :: { L Text }
var_id : varId { extractId $1 }

con_id :: { L Text }
con_id : conId { extractId $1 }

literal :: { Lit }
literal
  : integer  { LInt    (fromInteger $ unL $ extractInteger $ $1) }
  | double   { LDouble (unL $ extractDouble  $1) }
  | char     { LChar   (unL $ extractChar    $1) }
  | string   { LString (unL $ extractString  $1) }
  | boolean  { LBool   (unL $ extractBool    $1) }


-- -----------------------------------------------------------------------------
-- | Definitions

module_doc :: { Module }
module_doc : module_defn defns0 mayeof { Module $1 $2 }

mayeof :: { Maybe Token }
mayeof : {- Empty -} { Nothing }
       | EOF         { Just $1 }

module_defn :: { String }
module_defn : begin_line 'module' mod_name end_line { $3 }  

defns0 :: { [Defn] }
defns0
  : {- Empty -} { [] }
  | defns_r      { reverse $1 }

defns :: { [Defn] }
defns : defns_r { reverse $1 }

defns_r :: { [Defn] }
defns_r
  : defn_ln { [$1] }
  | defns_r defn_ln { $2 : $1 }

defn_ln :: { Defn }
defn_ln
  : begin_line defn end_line { $2 }

defn :: { Defn }
defn
  : func_defn_typed   { FuncDefn $1 }
  | extern_defn       { ExternDefn $1 }
  | datatype_defn     { DataTypeDefn $1 }


-- -----------------------------------------------------------------------------
-- | Function

func_defn_typed :: { Func }
func_defn_typed
  : func_sig end_line begin_line func_defn
    { let ((n1, ty), (n2, ps, body)) = ($1, $4)
      in if n1 == n2
          then func n1 ty ps body
          else error "Cannot parse function definition! Type and function name mismatch."  
    }

func_sig :: { (String, Type) }
func_sig : var_name ':' type { ($1, $3) }

func_defn :: { (String, [Pat], Exp) }
func_defn : var_name apats0 '=' exp { ($1, $2, $4) }


-- -----------------------------------------------------------------------------
-- | Patterns

pat :: { Pat }
pat : cpat { $1 }

cpat :: { Pat }
cpat
  : bpat ':' type { PType $1 $3 }
  | bpat { $1 }

bpat :: { Pat }
bpat
  : con_name apats { PCon $1 $2 }
  | apat { $1 }

apat :: { Pat }
apat : var_name    { pvar $1  }
     | con_name    { PCon $1 [] }
     | '_'         { PWild }
     | '(' pat ')' { $2 }

apats0 :: { [Pat] }
apats0
  : {- Empty -} { [] }
  | apats       { $1 }

apats :: { [Pat] }
apats
  : apats_r      { reverse $1 }

apats_r :: { [Pat] }
apats_r
  : apat { [$1] }
  | apats_r apat { $2:$1 }

-- -----------------------------------------------------------------------------
-- | Extern

extern_defn :: { Extern }
extern_defn : 'extern' var_name ':' type { let (paramtys, retty) = splitType $4
                                           in Extern $2 paramtys retty  } 

-- -----------------------------------------------------------------------------
-- | DataType

datatype_defn :: { DataType }
datatype_defn : 'type' con_name             { DataType $2 [] }
              | 'type' con_name '=' constrs { DataType $2 $4 }

constrs :: { [(String, [(Maybe String, Type)])] }
constrs : constrs_r { reverse $1 }

constrs_r :: { [(String, [(Maybe String, Type)])] }
constrs_r
  : constr               { [$1]  }
  | constrs_r '|' constr { $3:$1 }

constr :: { (String, [(Maybe String, Type)]) }
constr : con_name constr_body { ($1, $2) }

constr_body :: { [(Maybe String, Type)] }
constr_body : {- Empty -}   { [] }
            | constr_types  { $1 }
            | record_decl   { $1 }


constr_types :: { [(Maybe String, Type)] }
constr_types : constr_types_rev { reverse $1 }

constr_types_rev :: { [(Maybe String, Type)] }
constr_types_rev
  : atype                  { [(Nothing, $1)] }
  | constr_types_rev atype { (Nothing, $2):$1 }


record_decl :: { [(Maybe String, Type)] }
record_decl : '{' record_fields '}' { $2 }

record_fields :: { [(Maybe String, Type)] }
record_fields : record_fields_r { reverse $1 }

record_fields_r :: { [(Maybe String, Type)] }
record_fields_r
  : record_field                     { [$1]  }
  | record_fields_r ',' record_field { $3:$1 }

record_field :: { (Maybe String, Type) }
record_field 
  : var_name ':' type { (Just $1, $3) }
  | '_'      ':' type { (Nothing, $3) }


-- -----------------------------------------------------------------------------
-- | Expressions

exp :: { Exp }
exp : cexp { $1 }

dexp :: { Exp }
dexp
  : cexp ':' type { EType $1 $3 }
  | cexp { $1 }

cexp :: { Exp }
cexp
  : 'let' equations 'in' cexp { elet $2 $4 }
  | 'if' bexp 'then' exp elsebr { EIf $2 $4 $5 }
  | 'case' bexp 'of' clauses { ECase $2 $4 }
  | '\\' apats '->' exp { elam $2 $4 } 
  | bexp { $1 }

equations :: { [(Pat, Exp)] }
equations
  : begin_block equations_r end_block { reverse $2 }

equations_r :: { [(Pat, Exp)] }
equations_r
  : equation           { [$1] }
  | equations_r equation { $2:$1 }

equation :: { (Pat, Exp) }
equation : begin_line pat '=' exp end_line { ($2, $4) }

elsebr :: { Else }
elsebr
  : 'else' exp { Else $2 }
  | 'elif' bexp 'then' exp elsebr { Elif $2 $4 $5 }

clauses :: { [Clause] }
clauses
  : begin_block clauses_r end_block { reverse $2 }

clauses_r :: { [Clause] }
clauses_r
  : clause           { [$1] }
  | clauses_r clause { $2:$1 }

clause :: { Clause }
clause : begin_line pat '->' exp end_line { clause $2 $4 }

bexp :: { Exp }
bexp
  : aexp aexps { EApp $1 $2 }
  | con_name aexps0 { ECon $1 $2 }
  | 'new' con_name aexps0 { ENewCon $2 $3 }
  | 'delete' aexp { EFree $2 }
  | aexp { $1 }

aexp :: { Exp }
aexp : var_name          { evar $1 }
     | literal           { ELit $1 }
     | primOp            { EOp $1 }
     | '*' aexp          { ERef $2 }
     | '&' aexp          { EDeref $2 }
     | aexp '.' var_name { EGet $1 $3 }
     | '(' exp ')'       { $2 }

aexps0 :: { [Exp] }
aexps0
  : {- Empty -} { [] }
  | aexps       { $1 }

aexps :: { [Exp] }
aexps
  : aexps_r  { reverse $1 }

aexps_r :: { [Exp] }
aexps_r
  : aexp         { [$1] }
  | aexps_r aexp { $2:$1}


primOp :: { Op }
primOp
  : '#add' aexp aexp { OpAddI $2 $3 }
  | '#sub' aexp aexp { OpSubI $2 $3 }
  | '#mul' aexp aexp { OpMulI $2 $3 }
  | '#fadd' aexp aexp { OpAddF $2 $3 }
  | '#fsub' aexp aexp { OpSubF $2 $3 }
  | '#fmul' aexp aexp { OpMulF $2 $3 }

-- -----------------------------------------------------------------------------
-- | Types

type :: { Type }
type : btype { $1 }

btype :: { Type }
btype
  : atype '->' btype { TArr $1 $3 }
  | atype { $1 }

atype :: { Type }
atype : con_name     { TCon $1 }
      | primType     { $1 }
      | '*' atype    { TPtr $2 }
      | '(' type ')' { $2 }

atypes :: { [Type] }
atypes
  : atypes_r { reverse $1 }

atypes_r :: { [Type] }
atypes_r
  : atype { [$1] }
  | atypes_r atype { $2:$1 }


primType :: { Type }
primType
  : 'I8' { TI8 }
  | 'I32' { TI32 }
  | 'I64' { TI64 }
  | 'F32' { TF32 }
  | 'F64' { TF64 }
  | 'Bool' { TBool }
  | 'Char' { TChar }
  | 'String' { TString }
  | 'Void' { TVoid }

{

parseError :: [Token] -> a
parseError [] = error "Parse error, no tokens left!!"
parseError (t:_) = error $ "Parse error on token: " ++ show (pretty t)

}
