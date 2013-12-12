module EquationSolver.ManualParser(parseEquation, runParseEquation) where

  import EquationSolver.Common
  import Control.Monad.Writer
  import Control.Monad.Error
  import Control.Monad.Identity

  -- 1+2*3   ~> Plus (Mult (C 2) (C 3)) (C 1)
  -- 1*x+2*3 ~> Plus (Mult (C1) (V x)) (Mult (C 2) (C 3))
  -- x^2+x+2 ~> Plus (Pow (V x) (C 2)) (V x) (C 2)
  
  {--
    expr = factor * expr | factor / expr | factor
    factor = term + factor | term - factor | term
    term = number | variable | ( expr )
  --}

  import Data.Char  
  import EquationSolver.Common
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.State
  import Control.Monad.Writer

  -- Represents our tokens
  data Operation = LexPlus | LexMinus | LexMult | LexDiv | LexPow deriving (Eq)
  data Token = LexVar Char | LexConstant Integer | LexOp Operation | LexLParen | LexRParen | LexEquals  deriving (Eq)

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
      LexConstant (read digits :: Integer) <:> tokenize rest
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

  parseEquation :: String -> Flow Equation
  parseEquation expr = tokenize expr >>= parse'

  parse' :: [Token] -> Flow Equation
  parse' tokens =
    case break (== LexEquals) tokens of
      (l, [])  -> do
        -- no '=', we assume = 0
        le <- parseExpr l
        return $ Equation (fst le) (Constant 0)
      (l, _:r) -> do
        -- l '=' r
        le <- parseExpr l
        re <- parseExpr r
        return $ Equation (fst le) (fst re)

  -- expr = factor * expr | factor / expr | factor
  parseExpr :: [Token] -> Flow (Expr, [Token])
  parseExpr tkns = do
    (factor, restTkns) <- parseFactor tkns
    tell ["factor: " ++ (show factor) ++ ", rest: " ++ (show restTkns)]
    case restTkns of
      (LexOp LexMult):restExpr -> parseExpr restExpr >>= \expr -> return (Mult factor (fst expr), (snd expr))
      x                        -> return (factor, x)

    where
      -- factor = term + factor | term - factor | term
      parseFactor :: [Token] -> Flow (Expr, [Token])
      parseFactor tkns = do
        (term, restTkns) <- parseTerm tkns
        tell ["term: " ++ (show term) ++ ", rest: " ++ (show restTkns)]
        case restTkns of
          (LexOp LexPlus):restFactor -> parseFactor restFactor >>= \expr -> return (Plus term (fst expr), (snd expr))
          x                          -> return (term, x)

      -- term = number | variable | ( expr )
      parseTerm :: [Token] -> Flow (Expr, [Token])
      parseTerm tkns = do
        case tkns of
          (LexConstant c):restTkns -> return ((Constant c), restTkns)
          (LexVar x):restTkns      -> return ((Variable x), restTkns)
          (LexLParen):restTkns     -> remaining restTkns
        where
          remaining :: [Token] -> Flow (Expr, [Token])
          remaining tkns = do
              (expr, rest) <- parseExpr tkns
              case rest of
                LexRParen:rest' -> return (expr, rest')
                a:_             -> throwError "Unbalanced parens"
                []              -> throwError "Missing )"

  -- Convenience function for "eyeball debugging". Parses for example expressions
  -- runParse $ tokenize "x^2 + 4*x + 17 = 0"
  -- runParse $ tokenize "5*x*x + x + 6*x - 17 = 15*x"
  runParse' :: Flow [Token] -> Either String Equation
  runParse' tokenize = fst $ runIdentity $ runWriterT $ runErrorT $ tokenize >>= parse'

  runParseEquation :: String -> Either String Equation
  runParseEquation expr = runParse' $ tokenize expr

