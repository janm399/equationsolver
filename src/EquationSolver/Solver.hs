module EquationSolver.Solver where

  import EquationSolver.Common
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Identity
  import Control.Monad.Writer

  -- 1+2+3 Plus 1 (Plus 2 3)

  solveEquation :: Equation -> Flow [Double]
  solveEquation (Equation lhs rhs) = solve $ simplify $ reduce lhs rhs
    where
      solve :: Expr -> Flow [Double]
      solve (Minus (Variable x) (Constant c)) = return [fromIntegral c]
      solve (Plus (Variable x) (Constant c)) = return [fromIntegral (-c)]
      solve (Plus (Mult (Constant a) (Variable x)) (Constant c)) = return [(fromIntegral c) / (fromIntegral a)]
      solve x = do
        tell ["Don't know how solve " ++ (show x)]
        throwError $ "Fail at " ++ (show x) 

  reduce :: Expr -> Expr -> Expr
  reduce lhs rhs = Plus lhs (Mult (Constant (-1)) rhs)

  simplify :: Expr -> Expr
  simplify (Mult  (Constant a) (Constant b)) = Constant (a * b)
  simplify (Div   (Constant a) (Constant b)) = Constant (a `div` b)
  simplify (Plus  (Constant a) (Constant b)) = Constant (a + b)
  simplify (Minus (Constant a) (Constant b)) = Constant (a - b)

  simplify (Mult  (Constant 1) x) = simplify x
  simplify (Mult  x (Constant 1)) = simplify x
  simplify (Div   x (Constant 1)) = simplify x
  simplify (Plus  (Constant 0) x) = simplify x
  simplify (Plus  x (Constant 0)) = simplify x
  simplify (Pow   x (Constant 1)) = simplify x
  simplify (Pow   x (Constant 0)) = (Constant 1)

  simplify (Mult  a b) = simplify' a b Mult
  simplify (Div   a b) = simplify' a b Div
  simplify (Plus  a b) = simplify' a b Plus
  simplify (Minus a b) = simplify' a b Minus
  simplify (Pow   a b) = simplify' a b Pow
  simplify x = x

  simplify' :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Expr
  simplify' a b expr =
    let (sa, sb) = (simplify a, simplify b)
    in  if (sa /= a || sb /= b) then simplify $ expr sa sb else expr a b


  runSolveEquation :: Flow Equation -> Either String [Double]
  runSolveEquation equation = fst $ runIdentity $ runWriterT $ runErrorT $ equation >>= solveEquation