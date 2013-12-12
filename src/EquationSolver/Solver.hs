module EquationSolver.Solver(solveEquation, runSolveEquation) where

  import EquationSolver.Common
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Identity
  import Control.Monad.Writer

  solveEquation :: Equation -> Flow [Double]
  solveEquation (Equation lhs rhs) = solve $ simplify $ reduce lhs rhs
    where
      solve :: Expr -> Flow [Double]
      solve (Minus (Variable x) (Constant c)) = return [fromIntegral c]
      solve (Plus (Variable x) (Constant c)) = return [fromIntegral (-c)]
      solve x = do
        tell ["Don't know how solve " ++ (show x)]
        throwError "Fail"

  reduce :: Expr -> Expr -> Expr
  reduce lhs rhs = Plus lhs (Mult (Constant (-1)) rhs)

  simplify :: Expr -> Expr
  simplify (Mult (Constant a) (Constant b)) = Constant (a * b)
  simplify (Plus (Constant a) (Constant b)) = Constant (a + b)

  simplify (Mult a b) = Mult (simplify a) (simplify b)
  simplify (Plus a b) = Plus (simplify a) (simplify b)

  simplify x = x

  runSolveEquation :: Flow Equation -> Either String [Double]
  runSolveEquation equation = fst $ runIdentity $ runWriterT $ runErrorT $ equation >>= solveEquation