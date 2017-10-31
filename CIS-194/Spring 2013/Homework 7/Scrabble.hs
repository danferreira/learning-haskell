{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char

newtype Score = Score { getScore::Int }
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)


score :: Char -> Score
score l
        | l' `elem` "aeioulnstr" = Score 1
        | l' `elem` "dg" = Score 2
        | l' `elem` "bcmp" = Score 3
        | l' `elem` "fhvwy" = Score 4
        | l' `elem` "k" = Score 5
        | l' `elem` "jx" = Score 8
        | l' `elem` "qz" = Score 10
        | otherwise = Score 0
        where l' = toLower l

scoreString :: String -> Score
scoreString = foldr mappend mempty . map score
