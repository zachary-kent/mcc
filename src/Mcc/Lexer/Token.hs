module Mcc.Lexer.Token (Token (..)) where

import Data.Text (Text)
import Mcc.Ast.Type (Type)

-- | An Micro C token
data Token
  = Int Integer
  | Float Double
  | String Text
  | Char Int
  | Id Text
  | Type Type
  | Struct
  | Bool Bool
  | Null
  | Return
  | Assign
  | Comma
  | Semi
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | For
  | While
  | If
  | Else
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Not
  | BitAnd
  | BitOr
  | Pow
  | Dot
  | Arrow
  | Sizeof
