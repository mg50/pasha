module Analyze where
import qualified Data.Set as S
import Data.Set ((\\))
import Data.List (foldl')
import Types

data ProgramError = UndeclaredFunction String String
                  | UndeclaredVariable String String
                  | NoMain deriving (Show)

type Strings = S.Set String

analyze :: Program -> Either ProgramError Program
analyze program
  | hasNoMain           = Left NoMain
  | notNull undeclFuncs = Left $ uncurry UndeclaredFunction (head undeclFuncs)
  | notNull undeclVars  = Left $ uncurry UndeclaredVariable (head undeclVars)
  | otherwise           = Right program
  where funcNames = map funcName program
        hasNoMain = "main" `notElem` funcNames

        undeclFuncs = do f <- program
                         name <- S.toList $ undeclaredFunctions (S.fromList funcNames) f
                         return (funcName f, name)
        undeclVars  = do f <- program
                         v <- S.toList $ undeclaredVars f
                         return (funcName f, v)


undeclaredFunctions :: Strings -> Function -> Strings
undeclaredFunctions knownFunctionNames (Function _ _ body) =
  S.unions (map functionsInExpr body) \\ knownFunctionNames

functionsInExpr :: Expression -> Strings
functionsInExpr (Assignment _ body) = functionsInExpr body
functionsInExpr (FunctionCall f args) = S.insert f fs
  where fs = S.unions $ map functionsInExpr args
functionsInExpr _ = S.empty



undeclaredVars :: Function -> Strings
undeclaredVars (Function _ params body) = snd $ juxtAll go
  where go = (S.fromList params, S.empty) : map varsInExpr body

varsInExpr :: Expression -> (Strings, Strings)
varsInExpr (Variable v) = (S.empty, S.fromList [v])
varsInExpr StringLit{} = (S.empty, S.empty)
varsInExpr (Assignment v e) = let (decl, undecl) = varsInExpr e
                              in if S.member v undecl
                                    then (decl, undecl)
                                    else (S.insert v decl, undecl)
varsInExpr (FunctionCall _ es) = juxtAll $ map varsInExpr es

juxtAll :: (Ord a) => [(S.Set a, S.Set a)] -> (S.Set a, S.Set a)
juxtAll = foldl' juxt (S.empty, S.empty)

juxt :: (Ord a) => (S.Set a, S.Set a) ->
                   (S.Set a, S.Set a) ->
                   (S.Set a, S.Set a)
juxt (a1, b1) (a2, b2) = let a = S.union a1 a2
                             b = S.union b1 b2
                         in (a \\ b1, b \\ a1)

notNull :: [a] -> Bool
notNull = not . null
