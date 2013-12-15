module EquationSolver.Solver where

  import EquationSolver.Common
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Identity
  import Control.Monad.Writer
  import Data.List

  newtonReal :: Equation -> Flow [Double]
  newtonReal (Equation lhs rhs) =
    let lhs' = simplify lhs
        rhs' = simplify rhs
        vars = map head . group . sort $ extractVariables lhs' ++ extractVariables rhs'
    in  solve 0 (map (\v -> (v, -1000)) vars) lhs' rhs'
    where
      solve 1000 _    _   _   = throwError "No more iterations available."
      solve step vars lhs rhs =
        let lvalue = evaluate vars lhs
            rvalue = evaluate vars rhs
        in  if abs (rvalue - lvalue) > theta then 
              solve (step + 1) (map (\(x, v) -> (x, v + 0.1)) vars) lhs rhs
            else
              return $ map (\(x, v) -> v) vars
        where
          theta = 0

  evaluate :: [(Char, Double)] -> Expr -> Double
  evaluate substitution (Mult  a b)  = (evaluate substitution a) * (evaluate substitution b)
  evaluate substitution (Div   a b)  = (evaluate substitution a) / (evaluate substitution b)
  evaluate substitution (Plus  a b)  = (evaluate substitution a) + (evaluate substitution b)
  evaluate substitution (Minus a b)  = (evaluate substitution a) - (evaluate substitution b)
  evaluate substitution (Pow   a b)  = (evaluate substitution a) ** (evaluate substitution b)
  evaluate substitution (Sqrt  a)    = sqrt (evaluate substitution a)
  evaluate substitution (Variable x) = 
    case lookup x substitution of
      Just v -> v
  evaluate _ (Constant c)            = c

  substitute :: Expr -> Char -> Double -> Expr
  substitute (Mult  a b) x value = Mult  (substitute a x value) (substitute b x value)
  substitute (Div   a b) x value = Div   (substitute a x value) (substitute b x value)
  substitute (Plus  a b) x value = Plus  (substitute a x value) (substitute b x value)
  substitute (Minus a b) x value = Minus (substitute a x value) (substitute b x value)
  substitute (Pow   a b) x value = Pow   (substitute a x value) (substitute b x value)
  substitute (Sqrt  a)   x value = Sqrt  (substitute a x value)
  substitute v@(Variable x') x value 
    | x' == x   = (Constant value)
    | otherwise = v

  extractVariables :: Expr -> [Char]
  extractVariables (Mult  a b) = extractVariables a ++ extractVariables b
  extractVariables (Div   a b) = extractVariables a ++ extractVariables b
  extractVariables (Sqrt  a)   = extractVariables a
  extractVariables (Plus  a b) = extractVariables a ++ extractVariables b
  extractVariables (Minus a b) = extractVariables a ++ extractVariables b
  extractVariables (Pow   a b) = extractVariables a ++ extractVariables b
  extractVariables (Variable v) = [v]
  extractVariables _ = []

  algebraicReal :: Equation -> Flow [Double]
  algebraicReal (Equation lhs rhs) = solve $ simplify $ reduce lhs rhs
    where
      solve :: Expr -> Flow [Double]
      solve (Minus (Constant c) (Variable x)) = return [c]
      solve (Plus (Constant c) (Variable x)) = return [(-c)]
      solve (Plus (Constant c) (Mult (Constant a) (Variable x))) = return [c / a]
      solve (Plus (Constant c)
                  (Plus (Mult (Constant a) (Pow (Variable x1) (Constant 2)))
                        (Mult (Constant b) (Variable x2)))) = quadratic a b c
      solve (Plus (Constant c)
                  (Plus (Pow (Variable x1) (Constant 2))
                        (Mult (Constant b) (Variable x2)))) = quadratic 1 b c
      solve (Plus (Constant c)
                  (Plus (Pow (Variable x1) (Constant 2))
                        (Variable x2))) = quadratic 1 1 c
      solve (Plus (Pow (Variable x1) (Constant 2))
                  (Variable x2)) = quadratic 1 0 1
      solve x = do
        tell ["Don't know how solve " ++ (show x)]
        throwError $ "Fail at " ++ (show x) 

      quadratic a b c =
        let d = b * b - 4 * a * c
        in  if (d >= 0) then 
              return [(-b / 2 * a) + sqrt d, (-b / 2 * a) - sqrt d] 
            else 
              throwError "Complex solution"

  reduce :: Expr -> Expr -> Expr
  reduce lhs (Constant 0) = order lhs
  reduce lhs rhs = order $ Plus lhs (Mult (Constant (-1)) rhs)

  simplify :: Expr -> Expr
  -- constant collapse
  simplify (Mult  (Constant a) (Constant b)) = Constant (a * b)
  simplify (Div   (Constant a) (Constant b)) = Constant (a / b)
  simplify (Plus  (Constant a) (Constant b)) = Constant (a + b)
  simplify (Minus (Constant a) (Constant b)) = Constant (a - b)

  -- pow, /, *, +
  simplify (Mult  (Constant 1) x) = order $ simplify x
  simplify (Mult  (Constant 0) x) = Constant 0
  simplify (Div   x (Constant 1)) = order $ simplify x
  simplify (Plus  (Constant 0) x) = order $ simplify x
  simplify (Pow   x (Constant 1)) = order $ simplify x
  simplify (Pow   x (Constant 0)) = Constant 1

  simplify (Mult  a b) = order $ simplify' a b Mult
  simplify (Div   a b) = order $ simplify' a b Div
  simplify (Plus  a b) = order $ simplify' a b Plus
  simplify (Minus a b) = order $ simplify' a b Minus
  simplify (Pow   a b) = order $ simplify' a b Pow
  simplify x = x

  simplify' :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Expr
  simplify' a b expr =
    let (sa, sb) = (simplify a, simplify b)
    in  if (sa /= a || sb /= b) then simplify $ expr sa sb else expr a b

  order :: Expr -> Expr
  order (Mult  x c@(Constant _)) = Mult  c x
  order (Plus  x c@(Constant _)) = Plus  c x
  order (Minus x c@(Constant _)) = Minus c x
  order x = x


--  solveEquation :: Equation -> (Equation -> Flow a) Flow a
--  solveEquation equation method = met

  runSolveEquation :: Flow Equation -> (Equation -> Flow a) -> Either String a
  runSolveEquation equation method = fst $ runIdentity $ runWriterT $ runErrorT $ equation >>= method