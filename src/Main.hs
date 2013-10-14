module Main where
import Types
import Analyze
import Parser

main = return ()

-- buildProgram :: String -> Program
-- buildProgram p = case parsePasha p of
--   Left parseErr -> error $ "parse error: " ++ show parseErr
--   Right prog    -> case analyze prog of
--                      Left progError -> error $ "program error: " ++ show progError
--                      Right prog     -> prog
