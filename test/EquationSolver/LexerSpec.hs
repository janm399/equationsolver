module EquationSolver.LexerSpec (spec) where

  import Test.Hspec
  import EquationSolver.Lexer

  spec :: Spec
  spec = do
    describe "simple lexing" $ do
      
      it "lexes trivial expressions" $ do
        runTokenize "1+2" `shouldBe` Right [LexConstant 1, LexOp LexPlus, LexConstant 2]

      it "lexes more complex expressions" $ do
        runTokenize "2*(x+1)" `shouldBe` Right [LexConstant 2, LexOp LexMult, LexLParen, LexVar 'x', LexOp LexPlus, LexConstant 1, LexRParen]

      it "reports errors" $ do
        runTokenize "xx" `shouldBe` Left ("Bad syntax near 'x'")