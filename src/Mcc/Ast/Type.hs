module Mcc.Ast.Type (Type) where

import Data.Text (Text)

-- | The type of an expression in Micro C
data Type
  = Pointer Type
  | Int
  | Bool
  | Float
  | Char
  | Void
  | Struct Text
  deriving (Show, Eq)
