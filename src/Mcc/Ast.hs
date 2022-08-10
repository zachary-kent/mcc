module Mcc.Ast where

import Data.Char (chr)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Mcc.Ast.Type ( Type )
import qualified Mcc.Ast.Type as Type

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

-- | Unary operators
data Uop
  = Neg
  | Not
  deriving (Show, Eq)

data Expr
  = Int Integer
  | String Text
  | Char Integer
  | Float Double
  | Bool Bool
  | Null
  | Id Text
  | Binop Bop Expr Expr
  | Unop Uop Expr
  | Call Text [Expr]
  | Cast Type Expr
  | Dot Expr Expr
  | Deref Expr
  | Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  deriving (Show, Eq)

instance Num Expr where
  (+) = Binop Add
  (-) = Binop Sub
  (*) = Binop Mul
  negate = Unop Neg
  signum = undefined
  abs = undefined
  fromInteger = Int
