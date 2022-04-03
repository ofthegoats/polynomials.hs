module Types
  ( Polynomial,
    Term,
    Token (..),
  )
where

import qualified Data.Map as M

data Token = Constant Double | Variable | Minus | Plus | Power | Equals deriving (Show, Eq)

type Term = (Double, Double) -- (Power, Coefficient)

type Polynomial = M.Map Double Double -- Map Power Coefficient
