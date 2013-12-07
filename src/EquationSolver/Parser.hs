module EquationSolver.Parser where

  import EquationSolver.Flow
  import EquationSolver.Lexer
  import Control.Monad.Writer
  import Control.Monad.Error
  import Control.Monad.Identity

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
    expr = factor * expr | factor / expr | factor
    factor = term + factor | term - factor | term
    term = number | variable | ( expr )
  --}

  parse :: [Token] -> Flow Equation
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
  runParse' :: Flow [Token] -> (Either String Equation, [String])
  runParse' tokenize = runIdentity $ runWriterT $ runErrorT $ tokenize >>= parse

  runParse :: Flow [Token] -> Either String Equation
  runParse tokenize = fst $ runIdentity $ runWriterT $ runErrorT $ tokenize >>= parse

