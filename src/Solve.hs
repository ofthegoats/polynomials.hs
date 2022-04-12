module Solve (solvePolynomial) where

import Data.Complex
import qualified Data.Map as M
import GHC.List (iterate')
import Types (Polynomial)

{-
Given a polynomial in the form M.Map Power Coefficient
Find all (complex) solutions approximately
Using During-Kerner: https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method
-}

solvePolynomial :: Polynomial -> [Complex Double]
solvePolynomial p =
  let f = makeF p
      order = fst . head $ M.toDescList p
      base = [(0.4 :+ 0.9) ** (n :+ 0) | n <- [0 .. order - 1]]
      solutions =
        iterate'
          ( \(s : sols) ->
              iteration s sols f 0 (1 + length sols)
          )
          base
          !! 100
   in map (\(a :+ b) -> roundDP a 5 :+ roundDP b 5) solutions

makeF :: Polynomial -> Complex Double -> Complex Double
makeF poly x = M.foldlWithKey' (\y p c -> y + ((c :+ 0) * (x ** (p :+ 0)))) 0 poly

denom :: Complex Double -> [Complex Double] -> Complex Double
denom x rest = product $ map (x -) rest

iteration :: Complex Double -> [Complex Double] -> (Complex Double -> Complex Double) -> Int -> Int -> [Complex Double]
iteration s sols@(next : rest) f current max
  | current == max = s : sols
  | otherwise =
    let s' = s - f s / denom s sols
     in iteration next (rest ++ [s']) f (succ current) max

roundDP :: Double -> Int -> Double
roundDP x dp = (fromIntegral . round) (x * 10 ^ dp) / fromIntegral (10 ^ dp)
