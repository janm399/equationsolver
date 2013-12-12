{-# OPTIONS -XOverloadedStrings #-}
module EquationSolver.AttoparsecParser(parseEquation, runParseEquation) where
  import EquationSolver.Common
  import Control.Applicative
  import Control.Monad.Error
  import Data.ByteString 
  import Data.Attoparsec 
  import Data.Attoparsec.Expr as AE
  import qualified Data.Word8 as W8
  import qualified Data.ByteString.Char8 as C 
  import qualified Data.Attoparsec.Char8 as AC

  equation :: Parser Equation
  equation = do { lhs <- expr
                ; AC.char '='
                ; rhs <- expr 
                ; return $ Equation lhs rhs
                }
          <|> (expr >>= \lhs -> return $ Equation lhs (Constant 0))
          <?> "equation"

  expr :: Parser Expr
  expr = buildExpressionParser table factor
         <?> "expression"

  table = 
    [[pre "√" Sqrt],
     [inf "^" Pow AssocLeft]
    ,[inf "*" Mult AssocLeft, inf "/" Div AssocLeft]
    ,[inf "+" Plus AssocLeft, inf "-" Minus AssocLeft]
    ]          
    where
      inf s f assoc = Infix (do { string s; return f }) assoc
      pre s f = Prefix (do { string s; return f} )

  factor = do { AC.char '('
              ; x <- expr
              ; AC.char ')'
              ; return x 
              }
          <|> number
          <|> variable
          <?> "simple expression"

  number :: Parser Expr
  number = (AC.signed AC.decimal) >>= return . Constant
          <?> "number"

  variable :: Parser Expr
  variable = do { x <- AC.anyChar
                ; return $ Variable x
                }
            <?> "variable"

  parseEquation :: String -> Flow Equation
  parseEquation expr = case parseOnly equation (C.pack expr) of
    Right r -> return r
    Left msg -> throwError msg

  runParseEquation :: String -> Either String Equation
  runParseEquation expr = parseOnly equation (C.pack expr)