{-# OPTIONS -XOverloadedStrings #-}
module EquationSolver.AttoparsecParser(parseEquation, runParseEquation) where
  import EquationSolver.Common
  import Control.Applicative
  import Control.Monad.Error
  import Data.ByteString 
  import Data.Attoparsec 
  import Data.Attoparsec.Expr
  import qualified Data.Word8 as W8
  import qualified Data.ByteString.Char8 as C 
  import qualified Data.Attoparsec.Char8 as AC

  -- |The exposed function that takes the expression and returns @Flow Equation@
  parseEquation :: String         -- ^ The expression to be parsed
                -> Flow Equation  -- ^ Returned Flow monad transformer for Equation
  parseEquation expr = case runParseEquation expr of
    Right r -> return r
    Left msg -> throwError msg

  -- |Convenience function that runs the result of @parseEquation@, and returning the
  -- result as @Either String Equation@
  runParseEquation :: String                  -- ^ The expression to be parsed
                   -> Either String Equation  -- ^ The result of running @(parseEquation expr)@
  runParseEquation expr = parseOnly equation (C.pack expr)

  -- |Parser that matches the @equation@ defined using the grammar below:
  --
  -- @
  --     equation : expr ('=' expr)
  --     expr     : '√' factor | factor '^' factor | factor '/' factor 
  --                | factor '+' factor | factor '-' factor
  --     factor   : '(' expr ')' | number | variable
  --     number   : '-'? DIGIT+
  --     variable : [a-zA-Z]
  -- @
  equation :: Parser Equation
  equation = do { lhs <- expr
                ; AC.char '='
                ; rhs <- expr 
                ; return $ Equation lhs rhs
                }
          <|> (expr >>= \lhs -> return $ Equation lhs (Constant 0))
          <?> "equation"

  -- |Parser that matches the entire @expr@
  expr :: Parser Expr
  expr = buildExpressionParser table factor
     <?> "expression"

  -- |Returns a [[Ordering]], which defines the precedence of the
  -- operators; first comes √ (sqrt), then ^ (power), *, /, finally
  -- followed by + and -
  table = 
    [[pre "√" Sqrt],
     [inf "^" Pow AssocLeft]
    ,[inf "*" Mult AssocLeft, inf "/" Div AssocLeft]
    ,[inf "+" Plus AssocLeft, inf "-" Minus AssocLeft]
    ]          
    where
      inf s f = Infix  (do { string s; return f }) 
      pre s f = Prefix (do { string s; return f })

  -- |Parser that matches either @'(' expr ')'@ or just @number@ or @variable@
  factor = do { AC.char '('
              ; x <- expr
              ; AC.char ')'
              ; return x 
              }
          <|> number
          <|> variable
          <?> "simple expression"

  -- |Parser that matches signed decimal numbers
  number :: Parser Expr
  number = (AC.signed AC.decimal) >>= return . Constant
          <?> "number"

  -- |Parser that matches single alphas
  variable :: Parser Expr
  variable = AC.anyChar >>= return . Variable 
            <?> "variable"
