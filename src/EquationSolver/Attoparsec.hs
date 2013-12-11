{-# OPTIONS -XOverloadedStrings #-}
module EquationSolver.Attoparsec where
  import EquationSolver.Common
  import Data.ByteString 
  import qualified Data.ByteString.Char8 as C 
  import Data.Attoparsec
  import Control.Applicative
  import Data.Attoparsec.Expr
  import qualified Data.Attoparsec.Char8 as AC

  {--
    expr = factor * expr | factor / expr | factor
    factor = term + factor | term - factor | term
    term = number | variable | ( expr )
  --}

  m = Infix (mult) AssocLeft
  p = Infix (plus) AssocRight

  mult = do
    AC.char '*'
    return Mult

  plus = do
    AC.char '+'
    return Plus

  expr = buildExpressionParser [[m, p]] term

  term = 
        (AC.decimal >>= return . Constant)
    <|> (AC.char '(' >> expr)