{-# OPTIONS_GHC -Wall #-}

module Fibonacci where


-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

-- fibs2 :: [Integer]
-- fibs2 = 0 : 1 : f ([0..])
--     where
--       f (x:y:xs) = (x+y):f (y:(x+y):xs)

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =  Cons a (streamFromSeed f (f a))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) (0)

ruler :: Stream Integer
ruler = streamMap rulerAux nats

rulerAux :: Integer -> Integer
rulerAux n = last [x | x <- [1..n], (2*n) `mod` (2^x) == 0]
