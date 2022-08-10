module Mcc.Ast.Function (Function) where

import Data.Text (Text)
import Mcc.Ast (Stmt)
import Mcc.Ast.Binding (Binding)
import Mcc.Ast.Type (Type)

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
