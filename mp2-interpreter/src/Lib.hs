module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = maybe (ExnVal "No match in env") id (H.lookup s env)

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op intOps
    in if op == "/" && v2 == IntVal 0 then ExnVal "Division by 0" else liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op boolOps
    in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env 
        Just f = H.lookup op compOps 
    in liftCompOp f v1 v2

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    let comp = eval e1 env
    in if comp == BoolVal True then eval e2 env
       else if comp == BoolVal False then eval e3 env 
       else ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    let 
        evaled = map (\a -> eval a env) args
    in case eval e1 env of
        CloVal params body cloEnv -> eval body (insertion params evaled cloEnv)
        _ -> ExnVal "Apply to non-closure"
    where
        insertion [] [] env' = env'
        insertion (x:xs) (y:ys) env' = insertion xs ys (H.insert x y env')

--- ### Let Expressions

eval (LetExp pairs body) env = eval body (aux pairs env)
    where aux [] env = env
          aux ((x,y):xs) env = aux xs (H.insert x (eval y env) env)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, H.insert var (eval e env) env)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv,env)
exec (SeqStmt (x:xs)) penv env = aux (x:xs) penv env
    where aux [] penv env = ("", penv, env)
          aux (x:xs) penv env =
            let (str, penv', env') = exec x penv env
                (str2, penv2, env2) = aux xs penv' env'
            in (str ++ str2, penv2, env2)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = aux (eval e1 env) s1 s2 penv env
    where aux (BoolVal True) s1 s2 penv env = exec s1 penv env
          aux (BoolVal False) s1 s2 penv env = exec s2 penv env
          aux _ s1 s2 penv env = ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = maybe ("Procedure " ++ name ++ " undefined.", penv, env) aux (H.lookup name penv)
    where aux (ProcedureStmt _ procArgs body) = 
                let evaled = map (\a -> eval a env) args
                    newEnv = insertion procArgs evaled env
                in exec body penv newEnv
            where insertion [] [] env' = env'
                  insertion (x:xs) (y:ys) env' = insertion xs ys (H.insert x y env')
 