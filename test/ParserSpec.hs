module ParserSpec where
import Test.Hspec
import Types
import Parser
import Text.ParserCombinators.Parsec

spec = do
  describe "parsing expressions" $ do
    let p string = case parse parseExpression "" string of
                     Left  err -> error $ "expression parsing error: " ++ show err
                     Right val -> val
    it "parses a variable" $ do
      p "myVar" `shouldBe` Variable "myVar"

    it "parses a string" $ do
      p "'hello there'" `shouldBe` StringLit "hello there"

    it "parses an empty function call" $ do
      p "hello()" `shouldBe` FunctionCall "hello" []

    it "parses a function call with one string literal arg" $ do
      p "hello('asdf')" `shouldBe` FunctionCall "hello" [StringLit "asdf"]

    it "parses a function call with one variable argument" $ do
      p "hello(xyz)" `shouldBe` FunctionCall "hello" [Variable "xyz"]

    it "parses a function with two arguments" $ do
      p "hello(xyz, 'asdf')" `shouldBe` FunctionCall "hello" [ Variable "xyz"
                                                             , StringLit "asdf" ]

    it "parses multiple levels of function calls" $ do
      p "f(a, g(b, c))" `shouldBe` FunctionCall "f" [ Variable "a"
                                                    , FunctionCall "g" [ Variable "b"
                                                                       , Variable "c" ]]

  describe "parsing function definitions" $ do
    let p string = case parse parseFunctionDef "" string of
                     Left  err -> error $ "function def parsing error: " ++ show err
                     Right val -> val
    it "parses a function with no params" $ do
      p "def hello():\n  'asdf'" `shouldBe` Function "hello" [] [StringLit "asdf"]

    it "parses a function with no params and two statements" $ do
      p "def hello():\n  'asdf'\n  xyz" `shouldBe` Function "hello" [] [ StringLit "asdf"
                                                                      , Variable "xyz" ]

    it "parses a function with one param" $ do
      p "def hello(a):\n  a" `shouldBe` Function "hello" ["a"] [Variable "a"]

    it "parses a function with two params" $ do
      p "def hello(a, b):\n  a\n  b" `shouldBe` Function "hello" ["a", "b"] [ Variable "a"
                                                                           , Variable "b" ]

    it "handles weird spaces in parameters" $ do
      p "def hello( a, b,c):\n  a" `shouldBe` Function "hello" ["a", "b", "c"]
        [ Variable "a"]

    it "handles assignment statements" $ do
      p "def hello(a):\n  a = g('asdf')\n  b" `shouldBe` Function "hello" ["a"]

        [ Assignment "a" (FunctionCall "g" [StringLit "asdf"])
        , Variable "b" ]
