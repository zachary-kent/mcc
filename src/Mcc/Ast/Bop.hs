module Mcc.Ast.Bop (Bop (..)) where

-- | Binary operators
data Bop
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)
