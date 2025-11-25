module Expression where

data Expr = Const Double
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Cos Expr
          | Sin Expr

instance Eq Expr where
    (Const a) == (Const b) = a == b
    (Var a) == (Var b) = a == b
    (Add a1 b1) == (Add a2 b2) = (a1 == a2) && (b1 == b2)
    (Mul a1 b1) == (Mul a2 b2) = (a1 == a2) && (b1 == b2)
    (Sub a1 b1) == (Sub a2 b2) = (a1 == a2) && (b1 == b2)
    (Cos a) == (Cos b) = a == b
    (Sin a) == (Sin b) = a == b
    _ == _ = False

instance Show Expr where
    show (Const x) = show x
    show (Var x)   = x
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Cos x)   = "cos(" ++ show x ++ ")"
    show (Sin x)   = "sin(" ++ show x ++ ")"

type Context = [(String, Double)]

eval :: Context -> Expr -> Double
eval _ (Const c) = c
eval ctx (Var x) = 
    case lookup x ctx of
        Just val -> val
        Nothing  -> error ("Undefined variable :( : " ++ x)
eval ctx (Add a b) = eval ctx a + eval ctx b
eval ctx (Sub a b) = eval ctx a - eval ctx b
eval ctx (Mul a b) = eval ctx a * eval ctx b
eval ctx (Cos x)   = cos (eval ctx x)
eval ctx (Sin x)   = sin (eval ctx x)

simplify :: Expr -> Expr
simplify (Add a b) = 
    let sa = simplify a
        sb = simplify b
    in case (sa, sb) of
        (Const 0.0, r) -> r
        (l, Const 0.0) -> l
        (Const x, Const y) -> Const (x + y)
        _ -> Add sa sb
simplify (Mul a b) = 
    let sa = simplify a
        sb = simplify b
    in case (sa, sb) of
        (Const 0.0, _) -> Const 0.0
        (_, Const 0.0) -> Const 0.0
        (Const 1.0, r) -> r
        (l, Const 1.0) -> l
        (Const x, Const y) -> Const (x * y)
        _ -> Mul sa sb
simplify (Sub a b) =
    let sa = simplify a
        sb = simplify b
    in if sa == sb then Const 0.0 
       else case (sa, sb) of
            (l, Const 0.0) -> l
            (Const x, Const y) -> Const (x - y)
            _ -> Sub sa sb
simplify (Cos x) = Cos (simplify x)
simplify (Sin x) = Sin (simplify x)
simplify x = x

diff :: String -> Expr -> Expr
diff _ (Const _) = Const 0.0
diff var (Var x) = if x == var then Const 1.0 else Const 0.0
diff var (Add a b) = Add (diff var a) (diff var b)
diff var (Sub a b) = Sub (diff var a) (diff var b)
diff var (Mul u v) = Add (Mul (diff var u) v) (Mul u (diff var v))
diff var (Cos u) = Mul (Mul (Const (-1.0)) (Sin u)) (diff var u)
diff var (Sin u) = Mul (Cos u) (diff var u)

mainExpression :: IO ()
mainExpression = do
    putStrLn "Тест 1: Eval"
    let ctx1 = [("x", 10.0), ("y", 2.0)]
    let expr1 = Add (Var "x") (Mul (Var "y") (Const 3.0))
    
    putStrLn $ "Контекст: " ++ show ctx1
    putStrLn $ "Выражение: " ++ show expr1
    putStrLn $ "Результат: " ++ show (eval ctx1 expr1)

    putStrLn "\nТест 2: Eval"
    let exprTrig = Add (Cos (Const 0.0)) (Sin (Const 0.0))
    putStrLn $ "Выражение: " ++ show exprTrig
    putStrLn $ "Результат: " ++ show (eval [] exprTrig)

    putStrLn "\nТест 3: Simplify"
    let rawExpr = Add (Mul (Var "x") (Const 1.0)) (Mul (Var "y") (Const 0.0))
    putStrLn $ "Исходное: " ++ show rawExpr
    putStrLn $ "Упрощенное: " ++ show (simplify rawExpr)

    putStrLn "\nТест 4: Simplify"
    let complexTerm = Add (Var "x") (Const 5.0)
    let subSelf = Sub complexTerm complexTerm
    putStrLn $ "Исходное: " ++ show subSelf
    putStrLn $ "Упрощенное: " ++ show (simplify subSelf)

    putStrLn "\nТест 5: Diff"
    -- d/dx (x * x)
    let xSquare = Mul (Var "x") (Var "x")
    let dx = diff "x" xSquare
    putStrLn $ "f(x) = " ++ show xSquare
    putStrLn $ "f' сырая: " ++ show dx
    putStrLn $ "f' упрощ: " ++ show (simplify dx)

    putStrLn "\nТест 6: Diff"
    -- d/dx sin(2*x)
    let sin2x = Sin (Mul (Const 2.0) (Var "x"))
    let dSin = diff "x" sin2x
    putStrLn $ "f(x) = " ++ show sin2x
    putStrLn $ "f' сырая: " ++ show dSin
    putStrLn $ "f' упрощ: " ++ show (simplify dSin)