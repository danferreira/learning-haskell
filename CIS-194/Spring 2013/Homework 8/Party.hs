{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List (sort)

import Data.Tree
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (l ++ [e]) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL l1 f1) `mappend` (GL l2 f2) = GL (l1++l2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (withBoss, withoutBoss)
    where
        withBoss = foldr mappend (GL [b] (empFun b)) (map snd gls)
        withoutBoss = mconcat (map fst gls)

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5

format :: GuestList -> String
format (GL es fun) = "Total Fun: " ++ show fun ++ unlines (sort (map empName (es)))

main:: IO ()
main = do
        file <- readFile "company.txt"
        putStrLn (format $ maxFun $ read file)
