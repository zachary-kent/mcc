module Mcc.Ast.Program where

import Mcc.Ast.Binding (Binding)
import Mcc.Ast.Function (Function)
import Mcc.Ast.Struct (Struct)

-- | A Micro C program
data Program = Program
  { -- | The declared structs
    struct :: [Struct],
    -- | Global variables
    globals :: [Binding],
    -- | Defined functions
    functions :: [Function]
  }
