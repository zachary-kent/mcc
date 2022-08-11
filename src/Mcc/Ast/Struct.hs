module Mcc.Ast.Struct (Struct (..)) where

import Data.Text (Text)
import Mcc.Ast.Binding (Binding)

-- | A Micro C struct/record. 'Struct { name, fields = [Binding x1 t1, ...,
-- Binding xn tn)] }' represents the Micro C struct
-- 'struct name { t1 x1; ... ; tn xn; }'
data Struct = Struct
  { name :: Text,
    fields :: [Binding]
  }
