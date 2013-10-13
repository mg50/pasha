module InterpolateSpec where
import Test.Hspec
import Types
import Control.Monad
import Interpolate
import Text.ParserCombinators.Parsec

spec = do
  describe "parseInterpolant" $ do
    let ss       = StringSection
        vs       = VarSection
        p string = case parse parseInterpolant "" string of
                     Left err  -> error $ "interpolant parsing error: " ++ show err
                     Right val -> val
        testCases = [ ("hello", [ss "hello"])
                    , ("hello\\n", [ss "hello\\n"])
                    , ("#{asdf}", [vs "asdf"])
                    , ("#{asdf}hello", [vs "asdf", ss "hello"])
                    , ("hello#{asdf}world", [ss "hello", vs "asdf", ss "world"])
                    , ("hello\\#{asdf}", [ss "hello\\#{asdf}"])
                    , ("\\hello\\#{asdf}", [ss "\\hello\\#{asdf}"])
                  ]
    forM_ testCases $ \(str, expected) -> do
      it ("parses `" ++ str ++ "`") $ p str `shouldBe` expected

    -- it "parses `#{hello}`" $ do
    --   p "{hello}" `shouldBe` "hello"
