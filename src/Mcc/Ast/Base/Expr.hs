{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Mcc.Ast.Base.Expr where

import Data.Kind (Type)
import Data.Text (Text)
import Mcc.Ast.Bop (Bop)
import qualified Mcc.Ast.Type as Ast.Type
import Mcc.Ast.Uop (Uop)
import Mcc.Internal.Existential (Any)

data RValue

data LValue

type LHS :: Type -> Type -> Type
type family LHS p

type StructKey :: Type -> Type
type family StructKey p

type Node :: Type -> Type -> Type
type family Node p

-- | An expression in Micro C
type Expr :: Type -> Type -> Type
data Expr p a where
  Int :: Integer -> Expr p RValue
  String :: Text -> Expr p RValue
  Char :: Integer -> Expr p RValue
  Float :: Double -> Expr p RValue
  Bool :: Bool -> Expr p RValue
  Null :: Expr p RValue
  Id :: Text -> Expr p LValue
  Binop :: Bop -> Node p a -> Node p b -> Expr p RValue
  Unop :: Uop -> Node p a -> Expr p RValue
  Call :: Text -> [Any (Node p)] -> Expr p RValue
  Cast :: Type -> Expr p RValue
  Dot :: LNode p a -> StructKey p -> Expr p LValue
  Deref :: Expr p a -> Expr p LValue
  Addr :: LNode p a -> Expr p RValue
  Assign :: LNode p a -> Expr p RValue
  Sizeof :: Ast.Type.Type -> Expr p RValue

type LNode p a = Node p (LHS p a)
