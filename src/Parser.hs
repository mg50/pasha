module Parser where
import Types
import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

parseProgram :: Parser Program
parseProgram = do blankLines
                  defs <- parseFunctionDef `separatedBy` blankLines
                  return $ Program defs
  where blankLines = many $ many blank >> (void newline <|> eof)
        blank = satisfy $ \c -> isSpace c && c /= '\n'

parseFunctionDef :: Parser Function
parseFunctionDef = do string "def "
                      name <- many1 letter
                      char '('
                      paramNames <- parseParamNames
                      string "):"
                      newline
                      body <- many1 $ parseStatement
                      return $ Function name paramNames body

parseParamNames = many1 letter `separatedBy` char ','

parseStatement :: Parser Expression
parseStatement = do string "  "
                    expr <- parseExpression
                    void newline <|> eof
                    return expr

parseExpression = parseStringLit <|> parseFunctionOrAssignmentOrVariable

parseFunctionOrAssignmentOrVariable = do
  name <- many1 letter
  maybeParen <- optionMaybe $ char '('
  case maybeParen of
    Just _  -> do args <- parseExpression `separatedBy` char ','
                  char ')'
                  return $ FunctionCall name args
    Nothing -> parseAssignmentOrVariable name

parseAssignmentOrVariable :: String -> Parser Expression
parseAssignmentOrVariable varname = do
  many $ char ' '
  maybeEquals <- optionMaybe $ char '='
  case maybeEquals of
    Just _  -> do spaces
                  expr <- parseExpression
                  return $ Assignment varname expr
    Nothing -> return $ Variable varname

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy parser sep = do spaces
                            arg <- optionMaybe parser
                            allArgs <- case arg of
                                         Just a -> do rest <- parseRest
                                                      return $ a:rest
                                         Nothing -> return []
                            return allArgs
  where parseRest = do maybeSeparator <- optionMaybe sep
                       case maybeSeparator of
                         Just _  -> separatedBy parser sep
                         Nothing -> return []

parseStringLit :: Parser Expression
parseStringLit = do char '\''
                    lit <- many $ noneOf "'"
                    char '\''
                    return $ StringLit lit
