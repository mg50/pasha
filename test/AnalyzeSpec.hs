module AnalyzeSpec where
import Test.Hspec
import Types
import Analyze

spec = do
  describe "varsInExpr" $ do
    it "finds vars in a var" $ do
      varsInExpr (Variable "ab") `shouldBe` ([], ["ab"])

    it "finds vars in a string literal" $ do
      varsInExpr (StringLit "asdf") `shouldBe` ([], [])

    it "finds vars in an assignment" $ do
      varsInExpr (Assignment "ab" (Variable "cd")) `shouldBe` (["ab"], ["cd"])

    it "finds vars in a self-assignment" $ do
      varsInExpr (Assignment "ab" (Variable "ab")) `shouldBe` (["ab"], [])

    it "finds vars in a function call" $ do
      let expr = FunctionCall "f" [Variable "ab"]
      varsInExpr expr `shouldBe` ([], ["ab"])

    it "finds vars in a nested function call" $ do
      let expr = FunctionCall "f" [FunctionCall "g" [Variable "ab"]]
      varsInExpr expr `shouldBe` ([], ["ab"])

    it "finds vars in a nested function call with assignment" $ do
      let expr = FunctionCall "f" [Variable "cd"
                                  , Variable "ef"
                                  , FunctionCall "g" [Variable "ab"]]
      varsInExpr (Assignment "cd" expr) `shouldBe` (["cd"], ["ef", "ab"])

  describe "undeclaredVars" $ do
    it "finds no undeclared variable in a simple function" $ do
      let expr = Function "f" ["x"] [Variable "x"]
      undeclaredVars expr `shouldBe` []

    it "finds an undeclared variable in a slightly more compelex function" $ do
      let expr = Function "f" [] [Variable "x"]
      undeclaredVars expr `shouldBe` ["x"]

    it "finds undeclared variables in a complex function" $ do
      let expr = Function "f" ["a", "b"]
                   [ Variable "a"
                   , FunctionCall "g" [Variable "q"]
                   , Assignment "c" (Variable "a")
                   , Assignment "d" (Variable "e")]
      undeclaredVars expr `shouldBe` ["q", "e"]
