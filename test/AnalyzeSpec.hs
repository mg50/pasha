module AnalyzeSpec where
import Test.Hspec
import Types
import Analyze
import Helper
import qualified Data.Set as S

spec = do
  describe "varsInExpr" $ do
    let empty = [] :: [String]
    it "finds vars in a var" $ do
      varsInExpr (Variable "ab") `shouldBeEquiv` (empty, ["ab"])

    it "finds vars in a string literal" $ do
      varsInExpr (StringLit "asdf") `shouldBeEquiv` (empty, empty)

    it "finds vars in an assignment" $ do
      varsInExpr (Assignment "ab" (Variable "cd")) `shouldBeEquiv` (["ab"], ["cd"])

    it "finds vars in a self-assignment" $ do
      varsInExpr (Assignment "ab" (Variable "ab")) `shouldBeEquiv` (empty, ["ab"])

    it "finds vars in a function call" $ do
      let expr = FunctionCall "f" [Variable "a", Variable "b"]
      varsInExpr expr `shouldBeEquiv` (empty, ["a", "b"])

    it "finds vars in a nested function call" $ do
      let expr = FunctionCall "f" [FunctionCall "g" [Variable "ab"]]
      varsInExpr expr `shouldBeEquiv` (empty, ["ab"])

    it "finds vars in a nested function call with assignment" $ do
      let expr = FunctionCall "f" [Variable "cd"
                                  , Variable "ef"
                                  , FunctionCall "g" [Variable "ab"]]
      varsInExpr (Assignment "xy" expr) `shouldBeEquiv` (["xy"], ["ef", "ab", "cd"])

  describe "undeclaredVars" $ do
    let empty = [] :: [String]
    it "finds no undeclared variable in a simple function" $ do
      let expr = Function "f" ["x"] [Variable "x"]
      undeclaredVars expr `shouldBeEquiv` empty

    it "finds an undeclared variable in a slightly more compelex function" $ do
      let expr = Function "f" [] [Variable "x"]
      undeclaredVars expr `shouldBeEquiv` ["x"]

    it "finds undeclared variables in a complex function" $ do
      let expr = Function "f" ["a", "b"]
                   [ Variable "a"
                   , FunctionCall "g" [Variable "q"]
                   , Assignment "c" (Variable "a")
                   , Assignment "d" (Variable "e") ]
      undeclaredVars expr `shouldBeEquiv` ["q", "e"]

    it "finds undeclared variables when one variable is declared too late" $ do
      let expr = Function "f" [] [ Assignment "a" (Variable "b")
                                 , Assignment "b" (Variable "a") ]
      undeclaredVars expr `shouldBeEquiv` ["b"]


  describe "undeclaredFunctions" $ do
    it "finds undeclared function names" $ do
      let known = S.fromList ["f", "g"]
          f = Function "f" [] [ FunctionCall "f" [StringLit "a"]
                              , FunctionCall "h" [StringLit "b"]
                              , FunctionCall "i" [FunctionCall "j" [StringLit "c"]] ]
      undeclaredFunctions known f `shouldBeEquiv` ["h", "i", "j"]
