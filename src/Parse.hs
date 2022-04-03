{-# LANGUAGE LambdaCase #-}

module Parse (parsePolynomial) where

import Control.Applicative (liftA2)
import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import qualified Data.Map as M
import Text.Read (readMaybe)
import Types
  ( Polynomial,
    Term,
    Token (..),
  )

parsePolynomial :: String -> Maybe Polynomial
parsePolynomial s = tokenize s >>= simplify >>= makeTerms 1 >>= \x -> Just $ makePolynomial x

tokenize :: String -> Maybe [Token]
tokenize [] = Just []
tokenize ('-' : rest) = liftA2 (:) (Just Minus) (tokenize rest)
tokenize ('+' : rest) = liftA2 (:) (Just Plus) (tokenize rest)
tokenize ('^' : rest) = liftA2 (:) (Just Power) (tokenize rest)
tokenize ('=' : rest) = liftA2 (:) (Just Equals) (tokenize rest)
tokenize ('*' : rest) = tokenize rest -- multiplication is implied
tokenize (' ' : rest) = tokenize rest -- spaces are ignored
tokenize ('x' : rest) = liftA2 (:) (Just Variable) (tokenize rest) -- only x can be var
tokenize s@(h : rest)
  | isNumeric h = do
    let num = readMaybe (takeWhile isNumeric s) :: Maybe Double
    case num of
      Just d -> liftA2 (:) (Just (Constant d)) (tokenize (dropWhile isNumeric s))
      Nothing -> Nothing
  | otherwise = Nothing

simplify :: [Token] -> Maybe [Token]
simplify [] = Just []
simplify (Plus : Minus : rest) = simplify $ Minus : rest
simplify (Minus : Plus : rest) = simplify $ Minus : rest
simplify (Plus : Plus : rest) = simplify $ Plus : rest
simplify (Minus : Minus : rest) = simplify $ Plus : rest
simplify (a : rest) = liftA2 (:) (Just a) (simplify rest)

-- Double is used to determine whether we are on RHS or LHS
makeTerms :: Double -> [Token] -> Maybe [Term]
makeTerms _ [] = Just []
makeTerms _ [Equals] = Nothing -- cannot end in equals
makeTerms 1 (Equals : ts) = makeTerms (-1) ts
makeTerms (-1) (Equals : _) = Nothing -- cannot have 2 equal signs
makeTerms e (Plus : ts) =
  let term = takeWhile (\t -> t /= Minus && t /= Plus && t /= Equals) ts
      next = dropWhile (\t -> t /= Minus && t /= Plus && t /= Equals) ts
      c = getCoefficient e term
      p = getPower 0 term
   in liftA2 (:) (Just (p, c)) (makeTerms e next)
makeTerms e (Minus : ts) =
  let term = takeWhile (\t -> t /= Minus && t /= Plus && t /= Equals) ts
      next = dropWhile (\t -> t /= Minus && t /= Plus && t /= Equals) ts
      c = getCoefficient (-1 * e) term
      p = getPower 0 term
   in liftA2 (:) (Just (p, c)) (makeTerms e next)
makeTerms e ts = makeTerms e (Plus : ts)

makePolynomial :: [Term] -> Polynomial
makePolynomial =
  foldl'
    ( \m (p, c) ->
        M.alter
          ( \case
              Nothing -> Just c
              Just c' -> Just (c + c')
          )
          p
          m
    )
    M.empty

isNumeric :: Char -> Bool
isNumeric c = isDigit c || c == '.'

getCoefficient :: Double -> [Token] -> Double
getCoefficient c (Variable : Power : Constant _ : rest) = getCoefficient c rest
getCoefficient c (Constant c' : rest) = getCoefficient (c * c') rest
getCoefficient c (a : rest) = getCoefficient c rest
getCoefficient c [] = c

getPower :: Double -> [Token] -> Double
getPower p (Variable : Power : Constant p' : rest) = getPower (p + p') rest
getPower p (Variable : rest) = getPower (p + 1) rest
getPower p (a : rest) = getPower p rest
getPower p [] = p
