{
module Mcc.Parser.Generator where

import Mcc.Lexer.Token (Token)
import qualified Mcc.Lexer.Token as Token
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
  type      { Token.Type $$ }
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
  '['       { Token.LBrack }
  ']'       { Token.RBrack }
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
  '**'      { Token.Pow }
  '.'       { Token.Dot }
  '->'      { Token.Arrow }
  sizeof    { Token.Sizeof }

{
parseError :: [Token] -> a
parseError _ = error "Syntax Error"
}
