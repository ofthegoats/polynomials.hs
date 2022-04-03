module Main where

import Control.Monad (unless, when)
import Data.Complex
import qualified Data.Map as M
import Parse (parsePolynomial)
import Solve (solvePolynomial)
import Types (Polynomial, Term)

main :: IO ()
main = do
  inp <- getLine
  case parsePolynomial inp of
    Just poly -> do
      printPolynomial poly
      let sols = solvePolynomial poly
      mapM_
        ( \(n, s) ->
            putStrLn $
              "x_" ++ show n ++ " ≈ "
                ++ show (realPart s)
                ++ " + "
                ++ show (imagPart s)
                ++ "i"
        )
        $ zip [1 .. length sols] sols
    Nothing -> putStrLn "String provided is not in correct format for polynomial"

printPolynomial :: Polynomial -> IO ()
printPolynomial poly = do
  unless (M.null poly) $ printTerms $ M.toDescList poly
  putStrLn "= 0"

printTerms :: [Term] -> IO ()
printTerms = printTerms' 0

printTerms' :: Int -> [Term] -> IO ()
printTerms' _ [] = return ()
printTerms' count ((_, 0) : rest) = printTerms' count rest
printTerms' count ((p, c) : rest) = do
  if c < 0
    then putStr "-" >> when (count > 0) (putStr " ")
    else when (count > 0) (putStr "+ ")
  if c < 0
    then putStr . show $ -1 * c
    else putStr $ show c
  when (p /= 0) $ putStr $ "x^" ++ show p
  putStr " "
  printTerms' (succ count) rest
