module Mcc.Ast.Binding (Binding (..)) where

import Data.Text (Text)
import Mcc.Ast.Type (Type)

-- | A binding between an identifier and a type
data Binding = Binding
  { -- | The identifier of the binding
    id :: Text,
    -- | The type of the binding
    typ :: Type
  }
  deriving (Show, Eq)
