module Interpolate where
import Text.ParserCombinators.Parsec
import Control.Monad

data InterpolationSection = StringSection String | VarSection String deriving (Show, Eq)

interpolate :: [(String, String)] -> String -> String
interpolate pairs s =
  case parse parseInterpolant "" s of
    Left err       -> error $ "could not parse for interpolation: " ++ show err
    Right sections -> concatMap (substitute pairs) sections


substitute :: [(String, String)] -> InterpolationSection -> String
substitute _ (StringSection s) = s
substitute pairs (VarSection s) =
  case lookup s pairs of
    Just v  -> v
    Nothing -> error $ "could not find variable " ++ s ++ " to interpolate"

parseInterpolant :: Parser [InterpolationSection]
parseInterpolant = many $ parseVarSection <|> parseStringSection

parseVarSection :: Parser InterpolationSection
parseVarSection = do string "#{"
                     name <- many1 letter
                     char '}'
                     return $ VarSection name

parseStringSection :: Parser InterpolationSection
parseStringSection = liftM StringSection (parseStringSection' True)

parseStringSection' :: Bool -> Parser String
parseStringSection' b = do
  let comb = if b then many1 else many
  str <- comb $ noneOf "#\\"
  maybeEscaped <- optionMaybe escapedParser
  case maybeEscaped of
    Just esc -> do rest <- parseStringSection' False
                   return $ str ++ esc ++ rest
    Nothing  -> return str

escapedParser :: Parser String
escapedParser = do c <- char '\\'
                   maybeC' <- optionMaybe anyChar
                   return $ case maybeC' of
                              Just c' -> [c, c']
                              Nothing -> [c]