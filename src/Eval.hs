module Eval where
import Control.Monad
import Control.Monad.Trans.State
import Interpolate
import Types

eval :: Expression -> Pasha String
eval (Variable varname) = do
  bindings <- getBindings
  case lookup varname bindings of
    Just value -> return value
    Nothing    -> error $ "could not find variable " ++ varname

eval (StringLit s) = do
  bindings <- getBindings
  let s' = interpolate bindings s
  undefined

eval (Assignment v expr) = do
  result <- eval expr
  modify $ \e -> let b = bindings e
                 in e{bindings = (v, result):b}
  return result

eval (FunctionCall fname args) = do
  evalArgs <- mapM eval args
  fns <- getFns
  let (Function _ paramNames body) = lookupFn fname fns
  when (length paramNames /= length evalArgs) $
    error $ "incorrect number of parameters supplied for " ++ fname

  withBindings (zip paramNames evalArgs) $ do
    results <- mapM eval body
    case results of
      [] -> error $ "empty function body in function " ++ fname
      _  -> return (last results)

withBindings :: Bindings -> Pasha a -> Pasha a
withBindings bindings action = do
  env <- get
  let env' = env{bindings = bindings}
  put env'
  result <- action
  put env
  return result

lookupFn :: String -> Program -> Function
lookupFn fname [] = error $ "could not find function " ++ fname
lookupFn fname (f:fs) | fname == funcName f = f
                      | otherwise = lookupFn fname fs

getFns  = liftM fns get
getBindings = liftM bindings get
