module Mcc.Ast.Type
  ( Type (..),
    Primitive (..),
  )
where

import Data.Text (Text)

-- | A primitive (base) type
data Primitive
  = Int
  | Bool
  | Float
  | Char
  | Void
  deriving (Show, Eq)

-- | The type of an expression in Micro C
data Type
  = Primitive Primitive
  | Pointer Type
  | Struct Text
  deriving (Show, Eq)
