module Hw5 where

import           Data.Char
import           Data.List
import           Data.Ord

-- Exercise 1 --

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString = map (\c -> if isAscii c && not (isControl c) then c else '_')

holes :: [a] -> [[a]]
holes l = map (\x -> take x l ++ drop (succ x) l) [0..(pred (length l))]

longestText ::  Show a => [a] -> a
longestText = maximumBy (comparing (length . show))

adjacents :: [a] -> [(a,a)]
adjacents l = zip l (tail l)

commas :: [String] -> String
commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldr1 (zipWith (+))

sumNumbers :: String -> Integer
sumNumbers = toInteger . sum . map digitToInt . filter isDigit

-- Exercise 2 --

wordCount :: String -> String
wordCount s = unlines
             ["Number of lines: " ++ show (length ls),
              "Number of empty lines: " ++ show (length el),
              "Number of words: " ++ show (length ws),
              "Number of unique words: " ++ show (nub ws),
              "Number of words followed by themselves: " ++ show ft,
              "Length of the longest line: " ++ show (length (longestText ls))]
            where
              ls = lines s
              el = filter null ls
              ws = words s
              ft = length . filter (uncurry (==)) . adjacents $ ws


-- Exercise 3 --

testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens)
              , ("safeString",      ex_safeString)
              , ("holes",           ex_holes)
              , ("longestText",     ex_longestText)
              , ("commas",          ex_commas)
              , ("addPolynomials",  ex_addPolynomials)
              , ("sumNumbers",      ex_sumNumbers)
              ]

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "Thatâ€™s your line:\n" == "That_s your line:_"
    , safeString "ðŸ™‹.o(â€œMe Me Meâ€)" == "_.o(_Me Me Me_)"
    ]

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "OlÃ¡ mundo") ==  "OlÃ¡"
   ]

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]

failingTests :: [String] -> String
failingTests []     = ""
failingTests [x]    = x
failingTests [x,y]  = x ++ " and " ++ y
failingTests (x:xs) = x ++ ", " ++ failingTests xs

formatTest :: (String, [Bool]) -> String
formatTest (t, r) | and r = t ++ ": " ++ show total ++ "/" ++ show total ++ " successful tests"
              | or r = t ++ ": " ++ show passed ++"/"++ show total ++ " successful tests. Failing tests: " ++ failingTests failed
              | otherwise = t ++ ": " ++ "All " ++ show total ++ " tests failed."
        where
          total = length r
          passed = (length . filter id) r
          failed = map (show . fst) $ filter (not . snd) $ zip [1..length r] r

formatTests :: [(String, [Bool])] -> String
formatTests ts = unlines $ map formatTest ts

main :: IO ()
main = putStrLn $ formatTests testResults
