--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 f = f 1
factk x f = factk (x-1) (\t -> f (x * t))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ke ko = if even x then ke x else ko x
evenoddk (x:xs) ke ko = if even x then evenoddk xs (\t -> ke (x + t)) ko else evenoddk xs ke (\t -> ko (x+t))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp x) = True
isSimple (VarExp val) = True
isSimple (LamExp stringVal expression) = True
isSimple (IfExp ifcase left right) = isSimple left && isSimple right && isSimple ifcase
isSimple (OpExp op left right) = isSimple left && isSimple right
isSimple (AppExp _ _) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n = if isSimple e then (AppExp (AppExp f e) k, n)
                          else let (v, n') = gensym n
                                   v'      = VarExp v
                                   k'      = LamExp v (AppExp (AppExp f v') k)
                                in cpsExp e k' n'
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op l r) k n 
    | isSimple l && isSimple r = 
        (AppExp k (OpExp op l r), n)

    | isSimple l =
        let (v, n') = gensym n
            v'      = VarExp v
            k'      = LamExp v (AppExp k (OpExp op l v')) 
        in cpsExp r k' n'

    | isSimple r =
        let (v, n') = gensym n
            v'      = VarExp v
            k'      = LamExp v (AppExp k (OpExp op v' r)) 
        in cpsExp l k' n'
        
    | otherwise = 
        let (v, n')   = gensym n
            (v2, n'') = gensym n'
            k'        = LamExp v2 (AppExp k (OpExp op (VarExp v) (VarExp v2)))
            (k'',n''') = cpsExp r k' n''
            k'''       = LamExp v k''
        in cpsExp l k''' n'''
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp guard first second) k n
    | isSimple guard =
        let (v,n')   = cpsExp first k n
            (v2,n'') = cpsExp second k n'
        in (IfExp guard v v2, n'')
    | otherwise = 
        let (v, n')   = gensym n
            (f',n'')  = cpsExp first k n'
            (s',n''') = cpsExp second k n''
            k' = LamExp v (IfExp (VarExp v) f' s')
        in cpsExp guard k' n'''

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl fname args expression) =
    let newArgs = args ++ ["k"]
        kVar    = VarExp "k"
        (k,n)   = cpsExp expression kVar 1
    in Decl fname newArgs k