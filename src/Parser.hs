module Parser where
import Types
import Control.Monad
import Text.ParserCombinators.Parsec



parseFunctionDef :: Parser Function
parseFunctionDef = do string "def "
                      name <- many1 letter
                      char '('
                      paramNames <- parseParamNames
                      string "):"
                      newline
                      body <- many1 $ parseStatement
                      return $ Function name paramNames body

parseParamNames = parseArgs $ many1 letter

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
    Just _  -> do args <- parseArgs parseExpression
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

parseArgs :: Parser a -> Parser [a]
parseArgs parser = do spaces
                      arg <- optionMaybe parser
                      allArgs <- case arg of
                                   Just a -> do rest <- parseRestArgs parser
                                                return $ a:rest
                                   Nothing -> return []
                      spaces
                      return allArgs

parseRestArgs :: Parser a -> Parser [a]
parseRestArgs parser =  do maybeComma <- optionMaybe $ char ','
                           case maybeComma of
                             Just _  -> parseArgs parser
                             Nothing -> return []

parseVariable :: Parser Expression
parseVariable = do name <- many letter
                   return $ Variable name

parseStringLit :: Parser Expression
parseStringLit = do char '\''
                    lit <- many $ noneOf "'"
                    char '\''
                    return $ StringLit lit
