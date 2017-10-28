{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

import Control.Applicative (liftA2)


-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = (+) (eval e1) (eval e2)
eval (Mul e1 e2) =  (*) (eval e1) (eval e2)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
              Just expr -> Just (eval expr)
              Nothing -> Nothing

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x+y) 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x*y) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | VVar String
  deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 $ liftA2 (+)
  mul = liftA2 $ liftA2 (*)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
