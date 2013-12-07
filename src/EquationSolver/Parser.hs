module EquationSolver.Parser where

  import EquationSolver.Lexer
  import Control.Monad.Writer
  import Control.Monad.Error
  import Control.Monad.Identity

  type Parse a = ErrorT String (WriterT [String] Identity) a
  --type Parse a = ErrorT String Identity a
  data Equation = Equation Expr Expr deriving (Show, Eq)
  data Expr = Pow Expr Expr
            | Mult Expr Expr | Div Expr Expr
            | Plus Expr Expr | Minus Expr Expr
            | Variable Char
            | Constant Int deriving (Show, Eq)

  -- 1+2*3   ~> Plus (Mult (C 2) (C 3)) (C 1)
  -- 1*x+2*3 ~> Plus (Mult (C1) (V x)) (Mult (C 2) (C 3))
  -- x^2+x+2 ~> Plus (Pow (V x) (C 2)) (V x) (C 2)
  
  {--
    expr = factor * expr
         | factor / expr
         | factor

    factor = term + factor
           | term - factor
           | term

    term = number 
         | variable 
         | ( expr )
  --}


  parse :: [Token] -> Parse Equation
  parse tokens =
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

  parseExpr :: [Token] -> Parse (Expr, [Token])
  parseExpr tkns = do
    (factor, restTkns) <- parseFactor tkns
    tell ["factor: " ++ (show factor)]
    case restTkns of
      (LexOp LexMult):restExpr -> parseExpr restExpr >>= \expr -> return (Mult factor (fst expr), (snd expr))
      x                        -> return (factor, x)

    where
      parseFactor :: [Token] -> Parse (Expr, [Token])
      parseFactor tkns = do
        (term, restTkns) <- parseTerm tkns
        case restTkns of
          (LexOp LexPlus):restFactor -> parseTerm restFactor >>= \expr -> return (Plus term (fst expr), (snd expr))
          x                          -> return (term, x)

      parseTerm :: [Token] -> Parse (Expr, [Token])
      parseTerm tkns = do
        case tkns of
          (LexConstant c):restTkns -> return ((Constant c), restTkns)
          (LexVar x):restTkns      -> return ((Variable x), restTkns)
          (LexLParen):restTkns     -> parseExpr (init restTkns) >>= \expr -> return (fst expr, snd expr)
          _                        -> throwError $ "Unexpected " ++ (show tkns)

  -- Convenience function for "eyeball debugging". Parses for example expressions
  -- runParse $ tokenize "x^2 + 4*x + 17 = 0"
  -- runParse $ tokenize "5*x*x + x + 6*x - 17 = 15*x"
  runParse' :: Tokenize [Token] -> Either String (Equation, [String])
  runParse' tokenize = 
    runIdentity (runErrorT (runWriterT ("" tokenize >>= parse)))

  runParse :: Tokenize [Token] -> Either String Equation
  runParse tokenize = runIdentity (runErrorT $ tokenize >>= parse)

