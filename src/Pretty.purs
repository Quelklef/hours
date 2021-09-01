module Hours.Pretty (class Pretty, pretty) where

class Pretty a where
  pretty :: a -> String
