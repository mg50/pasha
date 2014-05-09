module Main where
import Types
import Analyze
import Parser
import Eval
import System.Environment (getArgs)
import System.IO

main = do [accessKey, secret, reward, lifetime, duration, fileLoc] <- getArgs
          withFile fileLoc ReadMode $ \hdl -> do
            fileContents <- hGetContents hdl
            case parsePasha fileContents of
              Left err -> error ("error!!! " ++ show err)
              Right program -> do
                putStrLn (show program)
                let config = Config accessKey secret 0.01 10 program 10 Sandbox
                result <- runPasha config $ eval (FunctionCall "main" [])
                putStrLn result

-- buildProgram :: String -> Program
-- buildProgram p = case parsePasha p of
--   Left parseErr -> error $ "parse error: " ++ show parseErr
--   Right prog    -> case analyze prog of
--                      Left progError -> error $ "program error: " ++ show progError
--                      Right prog     -> prog
