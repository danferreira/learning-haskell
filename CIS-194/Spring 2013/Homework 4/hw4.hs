{-# OPTIONS_GHC -Wall #-}

module Hw4 where

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
    where
      f x
        | even x = x `div` 2
        | otherwise = 3*x+1


-- Exercise 2

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr makeTree Leaf

makeTree :: a -> Tree a -> Tree a
makeTree a Leaf = Node 0 Leaf a Leaf
makeTree a (Node h l m r)
   | height l <= height r =
        let left = makeTree a l
        in  Node (height left + 1) left m r
   | otherwise =
        let right = makeTree a r
        in Node (height right + 1) l m right
  where
    height Leaf = -1
    height (Node ht _ _ _) = ht

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : [2*x + 1 | x <- exclude (n `div` 2)]

exclude :: Integer -> [Integer]
exclude n = [1..n] \\ [i + j + 2*i*j | (i, j) <- cartProd [1..n][1..n]]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
