{-# OPTIONS_GHC -Wall #-}

module Hw1 where

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
      | n <= 0 = []
      | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = reverse . double . reverse
--           where
--             double [] = []
--             double [x] = [x]
--             double (x:y:xs) = x:(2*y):double xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x:(2*y):(doubleEveryOther xs)

-- Exercise 3

sumDigits :: [Integer] -> Integer
--sumDigits = foldr ((+) . sum . toDigits) 0
sumDigits = foldr (\z y -> (sum $ toDigits z) + y) 0

-- Exercise 4

validate :: Integer -> Bool
validate = validate' . sumDigits . doubleEveryOther . toDigitsRev
      where
        validate' n = (n `mod` 10) == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n o d a
      | n == 0 = []
      | n == 1 = [(o, d)]
      | otherwise = (hanoi (n - 1) o a d) ++ [(o, d)] ++ (hanoi (n - 1) a d o)
