{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)


-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
            | i == 0 = Just a
            | otherwise = Nothing
indexJ i (Append m l r)
            | i >= sizemain = Nothing
            | i < sizel = indexJ i l
            | i >= (sizel) = indexJ (i - sizel) r
            | otherwise = Nothing
            where
              sizemain = getSize (size m)
              sizel = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m l r)
            | n >= sizemain = Empty
            | n <= sizel = dropJ n l +++ r
            | otherwise = dropJ (n - sizel) r
            where
              sizemain = getSize (size m)
              sizel = getSize $ size $ tag l


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m l r)
            | n >= sizemain = jl
            | n <= sizel = takeJ n l
            | otherwise = l +++ takeJ (n - sizel) r
            where
              sizemain = getSize (size m)
              sizel = getSize $ size $ tag l

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = foldr (+++) Empty . map (\s -> Single (scoreString s, Size 1) s) . lines
  line         = indexJ
  replaceLine n l b
                | n > (getSize $ snd $ tag b) = b
                | otherwise = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines     = getSize . snd . tag
  value        = getScore . fst . tag

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString $ unlines
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ]

main = runEditor editor initialBuffer















--
