module Stack where

import Data.List (lookup)

data Expr = Const Double
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Cos Expr
          | Sin Expr
          deriving (Eq)

instance Show Expr where
    show (Const x) = show x
    show (Var x)   = x
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Cos x)   = "cos(" ++ show x ++ ")"
    show (Sin x)   = "sin(" ++ show x ++ ")"

data Command = Push Double
             | PushVar String
             | OpAdd
             | OpSub
             | OpMul
             | OpSin
             | OpCos
             deriving (Eq, Show)

type Context = [(String, Double)]
type Stack = [Double]
type Result = Either String Stack

step :: Context -> Result -> Command -> Result
step _ (Left err) _ = Left err
step ctx (Right stack) cmd = case cmd of
    Push n -> Right (n : stack)
    
    PushVar x -> case lookup x ctx of
        Just val -> Right (val : stack)
        Nothing  -> Left $ "Undef var: " ++ x

    OpAdd -> binaryOp (+) stack
    OpSub -> binaryOp (-) stack
    OpMul -> binaryOp (*) stack
    OpSin -> unaryOp sin stack
    OpCos -> unaryOp cos stack

  where
    binaryOp op (x:y:rest) = Right ((op y x) : rest)
    binaryOp _ _           = Left "Stack underflow"

    unaryOp op (x:rest) = Right ((op x) : rest)
    unaryOp _ _         = Left "Stack underflow"

run :: Context -> [Command] -> Result
run ctx cmds = foldl (step ctx) (Right []) cmds


compile :: Expr -> [Command]
compile (Const x) = [Push x]
compile (Var x)   = [PushVar x]
compile (Add a b) = compile a ++ compile b ++ [OpAdd]
compile (Sub a b) = compile a ++ compile b ++ [OpSub]
compile (Mul a b) = compile a ++ compile b ++ [OpMul]
compile (Sin x)   = compile x ++ [OpSin]
compile (Cos x)   = compile x ++ [OpCos]


main :: IO ()
main = do
    putStrLn "Test 1: Manual execution"
    let ctx = [("x", 10.0), ("y", 2.0)]
    let prog = [PushVar "x", Push 3.0, PushVar "y", OpMul, OpAdd]
    
    putStrLn $ "Context: " ++ show ctx
    putStrLn $ "Program: " ++ show prog
    putStrLn $ "Result:  " ++ show (run ctx prog)

    putStrLn "\nTest 2: Compiler"
    let expr = Mul (Sub (Var "x") (Const 5.0)) (Var "y")
    let compiled = compile expr
    
    putStrLn $ "Expr: " ++ show expr
    putStrLn $ "Code: " ++ show compiled
    putStrLn $ "Run:  " ++ show (run ctx compiled)

    putStrLn "\nTest 3: Error handling"
    let errExpr = Add (Var "z") (Const 1.0)
    putStrLn $ "Run missing var: " ++ show (run ctx (compile errExpr))
    
    let errStack = [OpAdd]
    putStrLn $ "Run empty stack: " ++ show (run ctx errStack)