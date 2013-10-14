module Eval where
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import qualified Data.Map as M
import Interpolate
import Types

eval :: Expression -> Pasha String
eval (Variable varname) = do
  bindings <- get
  case M.lookup varname bindings of
    Just value -> return value
    Nothing    -> error $ "could not find variable " ++ varname

eval (StringLit s) = do
  bindings <- get
  let s' = interpolate (M.toList bindings) s
  undefined

eval (Assignment v expr) = do
  result <- eval expr
  modify (M.insert v result)
  return result

eval (FunctionCall fname args) = do
  fns <- ask
  let (Function _ paramNames body) = lookupFn fname fns
  when (length paramNames /= length args) $
    error $ "incorrect number of parameters supplied for " ++ fname

  evalArgs <- mapM eval args
  let newBindings = M.fromList $ zip paramNames evalArgs
  withState newBindings $ do
    results <- mapM eval body
    case results of
      [] -> error $ "empty function body in function " ++ fname
      _  -> return (last results)

withState :: (MonadState s m) => s -> m a -> m a
withState tempState action = do
  state <- get
  put tempState
  result <- action
  put state
  return result

lookupFn :: String -> Program -> Function
lookupFn fname [] = error $ "could not find function " ++ fname
lookupFn fname (f:fs) | fname == funcName f = f
                      | otherwise = lookupFn fname fs
