{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified ExprT as E
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: E.ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr x = case parseExp E.Lit E.Add E.Mul x of
            Just y -> Just (eval y)
            _ -> Nothing

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  mul = E.Mul
  add = E.Add

-- Exercise 4
instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>0)
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
  lit = Mod7
  mul (Mod7 x) (Mod7 y) = Mod7(mod (x*y) 7)
  add (Mod7 x) (Mod7 y) = Mod7(mod (x+y) 7)

-- Exercise 5
instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

--  Exercise 6
type MapExpr = 
  M.Map String Integer 
  -> Maybe Integer

class HasVars a where
  var :: String -> a

instance HasVars MapExpr where
    var = M.lookup

data VarExprT 
  = Lit Integer
  | Var String
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT

instance Expr MapExpr where
  lit x _ = Just x
  add f g = 
    \x -> case f x of
      Nothing -> Nothing
      Just a -> case g x of
        Nothing -> Nothing
        Just b -> Just (a + b)
  mul f g = 
    \x -> case f x of
      Nothing -> Nothing
      Just a -> case g x of
        Nothing -> Nothing
        Just b -> Just (a * b)

withVars 
  :: [(String, Integer)] 
  -> MapExpr 
  -> Maybe Integer
withVars vs x = x $ M.fromList vs
