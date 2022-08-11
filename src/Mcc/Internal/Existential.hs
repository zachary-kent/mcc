module Mcc.Internal.Existential (Any (..)) where

-- | An existential wrapper
data Any t where
  Any :: t a -> Any t
