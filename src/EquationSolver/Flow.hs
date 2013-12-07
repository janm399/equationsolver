module EquationSolver.Flow where
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Writer

  -- Tokenize of a is wraps the error message (String) over the Identity monad
  type Flow = ErrorT String (WriterT [String] Identity)
