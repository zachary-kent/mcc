module Mcc.Lexer.Token (Token (..)) where

import Data.Text (Text)
import Mcc.Ast.Type (Primitive)

-- | An Micro C token
data Token
  = Int Integer
  | Float Double
  | String Text
  | Char Int
  | Id Text
  | Type Primitive
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
  | For
  | While
  | If
  | Else
  | Add
  | Sub
  | Star
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
  | Dot
  | Arrow
  | Sizeof
