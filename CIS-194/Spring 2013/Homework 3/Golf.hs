{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips x = [skips' n x | n <- [0..(length x) - 1]]
          where
            skips' 0 xs = xs
            skips' n xs = case drop n xs of
                         [] -> []
                         (xl:xls) -> xl : skips' n xls

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
          | (x < y && y > z) = [y] ++ localMaxima (y:z:xs)
          | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = (unlines . reverse) [writeLine n | n <- [1..maxlines]] ++ footer
      where
        occurrences = [length $ filter (==x) xs | x <- [0..9]]
        maxlines = maximum occurrences
        writeLine n = [if x >= n then '*' else ' '| x <- occurrences]
        footer = "==========\n0123456789\n"
