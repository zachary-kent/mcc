module Mcc.Ast
  ( Bop (..),
    Uop (..),
    Literal (..),
    Expr (..),
    Stmt (..),
    Binding (..),
    Function (..),
    Struct (..),
    Program (..),
  )
where

import Data.Char (chr)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Mcc.Ast.Type (Type)
import qualified Mcc.Ast.Type as Type

-- | Binary operators
data Bop
  = Add
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
  | BitAnd
  | BitOr
  deriving (Show, Eq)

-- | Unary operators
data Uop
  = Neg
  | Not
  deriving (Show, Eq)

-- | Literal expressions
data Literal
  = Int Integer
  | String Text
  | Char Int
  | Float Double
  | Bool Bool
  | Null
  deriving (Show, Eq)

-- | An expression in Micro C
data Expr
  = Literal Literal
  | Id Text
  | Binop Bop Expr Expr
  | Unop Uop Expr
  | Call Text [Expr]
  | Cast Type Expr
  | Access Expr Expr
  | Deref Expr
  | Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  deriving (Show, Eq)

-- | A statement in Micro C
data Stmt
  = -- | An expression whose only purpose is its side effects
    Expr Expr
  | Block [Stmt]
  | Return (Maybe Expr)
  | -- | if (e) s
    If Expr Stmt
  | -- | if (e1) s1 else s2
    IfElse Expr Stmt Stmt
  | -- | for (e1; e2; e3) s
    For (Maybe Expr) (Maybe Expr) (Maybe Expr) Stmt
  | -- | while (e) s
    While Expr Stmt
  deriving (Show, Eq)

-- | A binding between an identifier and a type
data Binding = Binding
  { -- | The type of the binding
    typ :: Type,
    -- | The identifier of the binding
    id :: Text
  }
  deriving (Show, Eq)

-- | A function in Micro C
data Function = Function
  { -- | The return type of the function
    returnType :: Type,
    -- | The name of the function
    name :: Text,
    -- | The parameters to the function, described by a list of
    --   identifier - type bindings
    params :: [Binding],
    -- | Local variables defined having this function's scope
    locals :: [Binding],
    body :: [Stmt]
  }
  deriving (Show, Eq)

-- | A Micro C struct/record. 'Struct { name, fields = [Binding x1 t1, ...,
-- Binding xn tn)] }' represents the Micro C struct
-- 'struct name { t1 x1; ... ; tn xn; }'
data Struct = Struct
  { name :: Text,
    fields :: [Binding]
  }

-- | A Micro C program
data Program = Program
  { -- | The declared structs
    structs :: [Struct],
    -- | Global variables
    globals :: [Binding],
    -- | Defined functions
    functions :: [Function]
  }
