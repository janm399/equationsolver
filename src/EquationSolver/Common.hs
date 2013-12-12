module EquationSolver.Common where
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Writer

  -- |Flow collects errors, and allows to write tracing information to some type @x@
  -- (Remember, one could write @type Flow a = ErrorT String (...) a@.)
  type Flow = ErrorT String (WriterT [String] Identity)

  -- |Equation is a data type holding the left and right sides of the equation
  data Equation = Equation Expr Expr deriving (Show, Eq)

  -- |Expr defines operations we know about: exponentiation, (square) root, multiplication, addition,
  -- constants and variables
  data Expr = Pow Expr Expr | Sqrt Expr
            | Mult Expr Expr | Div Expr Expr
            | Plus Expr Expr | Minus Expr Expr
            | Variable Char
            | Constant Integer deriving (Show, Eq)

