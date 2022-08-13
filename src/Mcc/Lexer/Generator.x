{
module Mcc.Lexer.Generator where

import qualified Mcc.Ast.Type as Type
import Mcc.Lexer.Internal
import Mcc.Lexer.Token
}

%wrapper "basic-bytestring"

$alpha = [a-zA-Z]
$digit = 0-9
$newline = [\r\n]

tokens :-
  $white+ ;
  "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/" ;
  "//" [^$newline]* $newline ;
  digit+ { Int . readBytestring }
  $digit+ \. $digit* ( [eE] [\+\-]? $digit+ )?  { Float . readBytestring }
  \" [^\"]* \"                                  { String . readBytestring }
  \'^ [^\'\\] /\'                               { Char . firstChar }
  \'^ \\ $digit+ /\'                            { Char . escapedChar }
  "int"                                         { makeType Type.Int }
  "bool"                                        { makeType Type.Bool }
  "float"                                       { makeType Type.Float }
  "char"                                        { makeType Type.Char }
  "void"                                        { makeType Type.Void }
  "struct"                                      { const Struct }
  "true"                                        { makeBool True }
  "false"                                       { makeBool False }
  "null"                                        { const Null }
  "return"                                      { const Return }
  \=                                            { const Assign }
  \,                                            { const Comma }
  \;                                            { const Semi }
  \(                                            { const LParen }
  \)                                            { const RParen }
  \{                                            { const LBrace }
  \}                                            { const RBrace }
  "for"                                         { const For }
  "while"                                       { const While }
  "if"                                          { const If }
  "else"                                        { const Else }
  \+                                            { const Add }
  \-                                            { const Sub }
  \*                                            { const Star }
  \/                                            { const Div }
  "=="                                          { const Equal }
  "!="                                          { const Neq }
  \>                                            { const Greater }
  ">="                                          { const Geq }
  "&&"                                          { const And }
  "||"                                          { const Or }
  \!                                            { const Not }
  \&                                            { const BitAnd }
  \|                                            { const BitOr }
  \.                                            { const Dot }
  "->"                                          { const Arrow }
  "sizeof"                                      { const Sizeof }
  $alpha [$alpha $digit \_]*                    { Id . decode }
