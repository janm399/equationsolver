module EquationSolver.Lexer where

  import Data.Char  
  import EquationSolver.Flow
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Writer

  -- Represents our tokens
  data Operation = LexPlus | LexMinus | LexMult | LexDiv | LexPow deriving (Eq)
  data Token = LexVar Char | LexConstant Int | LexOp Operation | LexLParen | LexRParen | LexEquals  deriving (Eq)

  -- Show instances to make the tokens look nice
  instance Show Operation where
    show LexPlus = "+"
    show LexMinus = "-"
    show LexMult = "*"
    show LexDiv = "/"
    show LexPow = "^"

  -- Show instances to make the tokens look nice
  instance Show Token where
    show (LexVar v) = [v]
    show (LexConstant i) = show i
    show (LexOp op) = show op
    show (LexLParen) = "("
    show (LexRParen) = ")"
    show LexEquals = "="

  -- simple tokenizer that breaks our stream into digits, operators, variables, parens and equals
  tokenize :: String -> Flow [Token]
  tokenize expr@(x:xs) 
    | isDigit x    = do
      let (digits, rest) = span isDigit expr -- consume this and all other digits
      LexConstant (read digits :: Int) <:> tokenize rest
    | isAlpha x    = do
      case span isAlpha expr of -- consume this and all other letters
        (var:[], rest) -> LexVar var <:> tokenize rest
        _              -> syntaxError x 
    | isOperator x = operator x <:> tokenize xs
    | isParen x    = paren x <:> tokenize xs
    | isWs x       = tokenize xs
    | isEquals x   = LexEquals <:> tokenize xs
    | otherwise    = syntaxError x
  tokenize []      = return []

  (<:>) :: Token -> Flow [Token] -> Flow [Token]
  (<:>) token tokenizeRest = tokenizeRest >>= \rest -> return $ token : rest

  -- constructs operator from x
  operator :: Char -> Token
  operator x = LexOp $ case x of
                  '*' -> LexMult
                  '/' -> LexDiv
                  '^' -> LexPow
                  '+' -> LexPlus
                  '-' -> LexMinus
  paren ')' = LexRParen
  paren '(' = LexLParen 

  -- prepares syntax error
  syntaxError :: (Show a) => a -> Flow b
  syntaxError x = throwError $ "Bad syntax near " ++ (show x)

  -- simple character matchers
  isOperator x = elem x "+-/*^"
  isParen x = elem x "()"
  isWs x = elem x " \t\n"
  isEquals x = x == '='

  -- Convenience "eyeball debugging function". Lexes for example
  -- "x^2 + 4*x + 17 = 0"
  -- "5*(x*x + x) + 6*x - 17 = 15*x"
  runTokenize :: String -> Either String [Token]
  runTokenize x = fst $ runIdentity $ runWriterT $ runErrorT $ tokenize x
