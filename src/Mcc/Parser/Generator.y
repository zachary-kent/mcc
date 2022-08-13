{
module Mcc.Parser.Generator where

-- import Data.Function
import Lens.Micro.Platform
import Mcc.Ast
import Mcc.Lexer.Token (Token)
import qualified Mcc.Lexer.Token as Token
import Mcc.Ast.Type (Type)
import qualified Mcc.Ast.Type as Type
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int       { Token.Int $$ }
  float     { Token.Float $$ }
  string    { Token.String $$ }
  char      { Token.Char $$ }
  id        { Token.Id $$ }
  ptype     { Token.Type $$ }
  struct    { Token.Struct }
  bool      { Token.Bool $$ }
  null      { Token.Null }
  return    { Token.Return }
  '='       { Token.Assign }
  ','       { Token.Comma }
  ';'       { Token.Semi }
  '('       { Token.LParen }
  ')'       { Token.RParen }
  '{'       { Token.LBrace }
  '}'       { Token.RBrace }
  for       { Token.For }
  while     { Token.While }
  if        { Token.If }
  else      { Token.Else }
  '+'       { Token.Add }
  '-'       { Token.Sub }
  '*'       { Token.Star }
  '/'       { Token.Div }
  '=='      { Token.Equal }
  '!='      { Token.Neq }
  '<'       { Token.Less }
  '<='      { Token.Leq }
  '>'       { Token.Greater }
  '>='      { Token.Geq }
  '&&'      { Token.And }
  '||'      { Token.Or }
  '!'       { Token.Not }
  '&'       { Token.BitAnd }
  '|'       { Token.BitOr }
  '.'       { Token.Dot }
  '->'      { Token.Arrow }
  sizeof    { Token.Sizeof }

%nonassoc if
%nonassoc else
%right '='
%left '|'
%left '&'
%left '||'
%left '&&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
%right '!' NEG
%left '.' '->'

%%

program :: { Program }
  : rev_decls { let (structs, globals, functions) = $1 
                in Program (reverse structs) (reverse globals) (reverse functions) }

rev_decls :: { ([Struct], [Binding], [Function]) }
  : {- empty -}         { ([], [], []) }
  | rev_decls struct_decl   { $1 & _1 %~ ($2 :) }
  | rev_decls var_decl      { $1 & _2 %~ ($2 :) }
  | rev_decls function_decl { $1 & _3 %~ ($2 :) }

struct_decl :: { Struct }
  : struct id '{' var_decls '}' ';' { Struct $2 $4 }

function_decl :: { Function }
  : type id '(' params ')' '{' var_decls stmts '}' { Function $1 $2 $4 $7 $8 }

params :: { [Binding] }
  : rev_params { reverse $1 }

rev_params :: { [Binding] }
  : {- empty -}             { [] }
  | binding                 { [$1] }
  | rev_params ',' binding  { $3 : $1 }

binding :: { Binding }
  : type id { Binding $1 $2 }

type :: { Type }
  : ptype     { Type.Primitive $1 }
  | struct id { Type.Struct $2 }
  | type '*'  { Type.Pointer $1 }

var_decls :: { [Binding] }
  : rev_var_decls { reverse $1 }

rev_var_decls :: { [Binding] }
  : {- empty -}               { [] }
  | rev_var_decls var_decl    { $2 : $1 }

var_decl :: { Binding }
  : binding ';' { $1 }

stmts :: { [Stmt] }
  : rev_stmts { reverse $1 }

rev_stmts :: { [Stmt] }
  : {- empty -}     { [] }
  | rev_stmts stmt  { $2 : $1 }

stmt :: { Stmt }
  : expr ';'                                            { Expr $1 }
  | return expr_opt ';'                                 { Return $2 }
  | '{' stmts '}'                                       { Block $2 }
  | if '(' expr ')' stmt %prec if                       { If $3 $5 }
  | if '(' expr ')' stmt else stmt                      { IfElse $3 $5 $7 }
  | for '(' expr_opt ';' expr_opt ';' expr_opt ')' stmt { For $3 $5 $7 $9 }
  | while '(' expr ')' stmt                             { While $3 $5 }

expr_opt :: { Maybe Expr }
  : {- empty -} { Nothing }
  | expr        { Just $1 }

expr :: { Expr }
  : literal                     { Literal $1 }
  | expr '+'  expr              { Binop  Add $1 $3 }
  | expr '-'  expr              { Binop  Sub $1 $3 }
  | expr '*'  expr              { Binop  Mul $1 $3 }
  | expr '/'  expr              { Binop  Div $1 $3 }
  | expr '==' expr              { Binop  Equal $1 $3 }
  | expr '!=' expr              { Binop  Neq $1 $3 }
  | expr '<'  expr              { Binop  Less $1 $3 }
  | expr '<=' expr              { Binop  Leq $1 $3 }
  | expr '>'  expr              { Binop  Greater $1 $3 }
  | expr '>=' expr              { Binop  Geq $1 $3 }
  | expr '&'  expr              { Binop  BitAnd $1 $3 }
  | expr '|'  expr              { Binop  BitOr $1 $3 }
  | expr '&&' expr              { Binop  And $1 $3 }
  | expr '||' expr              { Binop  Or $1 $3 }
  | '-' expr %prec NEG          { Unop Neg $2 }
  | '*' expr  %prec NEG         { Deref $2 }
  | '&' expr %prec NEG          { Addr $2 }
  | '!' expr                    { Unop Not $2 }
  | expr '=' expr               { Assign $1 $3 }
  | id '(' exprs ')'            { Call $1 $3 }
  | '(' type ')' expr %prec NEG { Cast $2 $4 }
  | expr '.' expr               { Access $1 $3 }
  | expr '->' expr              { Access (Deref $1) $3}
  | sizeof '(' type ')'         { Sizeof $3 }
  | '(' expr ')'                { $2 }

exprs :: { [Expr] }
  : rev_exprs { reverse $1 }

rev_exprs :: { [Expr] }
  : {- empty -}         { [] }
  | expr                { [$1] }
  | rev_exprs ',' expr  { $3 : $1 }

literal :: { Literal }
  : int     { Int $1 }
  | string  { String $1 }
  | char    { Char $1 }
  | float   { Float $1 }
  | bool    { Bool $1 }
  | null    { Null }

{
parseError :: [Token] -> a
parseError = const $ error "Syntax Error"
}
