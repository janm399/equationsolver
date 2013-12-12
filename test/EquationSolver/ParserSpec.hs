module EquationSolver.ParserSpec (spec) where

  import Test.Hspec
  import qualified EquationSolver.ManualParser as MP
  import qualified EquationSolver.AttoparsecParser as AP
  import EquationSolver.Common

  spec :: Spec
  spec = do
    describe "simple parsing" $ do
      runSpec "Attoparsec parser" AP.runParseEquation
      runSpec "Manual parser" MP.runParseEquation

    where
      runSpec :: String -> (String -> Either String Equation) -> Spec
      runSpec descr pat = do
        describe descr $ do
          it "parses trivial expressions" $ do
            (pat "1+2") `shouldBe` (Right (Equation (Plus (Constant 1) (Constant 2)) (Constant 0)))

          it "parses trivial expressions" $ do
            (pat "1*2") `shouldBe` (Right (Equation (Mult (Constant 1) (Constant 2)) (Constant 0)))

          it "parses trivial expressions" $ do
            (pat "(1*2)") `shouldBe` (Right (Equation (Mult (Constant 1) (Constant 2)) (Constant 0)))

          it "deals with braces I" $ do
            (pat "1*(2+3)") `shouldBe` (Right (Equation (Mult (Constant 1) (Plus (Constant 2) (Constant 3))) (Constant 0)))

          it "deals with braces II" $ do
            (pat "1+(2*3)") `shouldBe` (Right (Equation (Plus (Constant 1) (Mult (Constant 2) (Constant 3))) (Constant 0)))

          it "deals with braces III" $ do
            (pat "(1*2)+3") `shouldBe` (Right (Equation (Plus (Mult (Constant 1) (Constant 2)) (Constant 3)) (Constant 0)))

          it "deals with precedence properly" $ do
            (pat "1*2+3") `shouldBe` (Right (Equation (Plus (Mult (Constant 1) (Constant 2)) (Constant 3)) (Constant 0)))
