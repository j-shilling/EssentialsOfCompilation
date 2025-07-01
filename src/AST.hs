{-# LANGUAGE GADTs #-}

-- | Defines the internal representation of Scheme values

module AST
  ( Expression(..)
  , Prim(..)
  )
where

data Prim where
  Read :: Prim
  Neg :: Expression -> Prim
  Sub :: Expression -> Expression -> Prim
  Add :: Expression -> Expression -> Prim
  deriving (Show, Eq)

data Expression where
  Int :: Integer -> Expression
  Prim :: Prim -> Expression
  deriving (Show, Eq)
