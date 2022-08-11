module Mcc.Ast.Uop (Uop (..)) where

-- | Unary operators
data Uop
  = Neg
  | Not
  deriving (Show, Eq)
