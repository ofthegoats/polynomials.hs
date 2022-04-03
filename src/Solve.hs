module Solve (solvePolynomial) where

import Data.Complex
import qualified Data.Map as M
import Types (Polynomial)

{-
Given a polynomial in the form M.Map Power Coefficient
Find all the (real and complex) solutions approximately
Using During-Kerner: https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method
-}

solvePolynomial :: Polynomial -> [Complex Double]
solvePolynomial p = [1 :+ 1]
