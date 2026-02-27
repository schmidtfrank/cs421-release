--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

import Prelude as P
--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons a next) = a : cons2list next

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp num) = num
eval (PlusExp []) = 0
eval (MultExp []) = 1
eval (PlusExp xs) = P.foldl (\acc x -> acc + x) 0 (P.map (eval) xs)
eval (MultExp xs) = P.foldl (\acc x -> acc * x) 1 (P.map (eval) xs)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' [] = Nil
list2cons' (x:xs) = P.foldr (Cons) Nil (x:xs)

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
  deriving (Show, Eq)
--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node val left right) = val + sumTree left + sumTree right


--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer 
                | BoolVal Bool
                | StrVal String
                | ExnVal String
  deriving (Show)
--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal(op x y)
liftIntOp op _ (IntVal y) = ExnVal ("not an IntVal!")
liftIntOp op (IntVal x) _ = ExnVal ("not an IntVal!")
liftIntOp _ _ _ = ExnVal ("not an IntVal!")