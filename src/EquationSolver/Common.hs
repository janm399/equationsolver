module EquationSolver.Common where
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Writer

  -- Tokenize of a is wraps the error message (String) over the Identity monad
  type Flow = ErrorT String (WriterT [String] Identity)
  data Equation = Equation Expr Expr deriving (Show, Eq)
  data Expr = Pow Expr Expr
            | Mult Expr Expr | Div Expr Expr
            | Plus Expr Expr | Minus Expr Expr
            | Variable Char
            | Constant Integer deriving (Show, Eq)

