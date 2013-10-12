module Analyze where
import Data.List ((\\))
import qualified Data.List as L
import Types

-- allCalledFunctionNames :: Program -> [String]
-- allCalledFunctionNames (Program []) = []
-- allCalledFunctionNames (Program fs) = concatMap go fs
--   where go (Function _ params body) = concatMap (names params) body
--         names _ (Assignment _ expr) = names expr
--         names shadowed (FunctionCall f args) = f : [name | expr <- args
--                                                          , name <- names shadowed expr
--                                                          , not (name `elem` shadowed)]
--         names _ _ = []



undeclaredVars :: Function -> [String]
undeclaredVars (Function _ params body) = go params body
  where go declared [] = []
        go declared (e:es) = let (decl, undecl) = varsInExpr e
                                 diff = undecl \\ declared
                                 declared' = L.nub $ declared ++ decl
                             in diff ++ go declared' es

varsInExpr :: Expression -> ([String], [String])
varsInExpr (Variable v) = ([], [v])
varsInExpr StringLit{} = ([], [])
varsInExpr (Assignment v e) = let (decl, undecl) = varsInExpr e
                              in (L.nub (v:decl), undecl `without` v)
varsInExpr (FunctionCall _ es) = foldr join ([], []) $ map varsInExpr es
  where join (as, bs) (cs, ds) = (as ++ cs, bs ++ ds)


without :: (Eq a) => [a] -> a -> [a]
without xs y = [x | x <- xs, x /= y]
