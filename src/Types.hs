{-# LANGUAGE GADTs #-}
module Types where
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

type Program = [Function]

data Expression = Variable String
                | StringLit String
                | Assignment String Expression
                | FunctionCall String [Expression] deriving (Eq, Show)

data Function = Function { funcName :: String
                         , funcParams :: [String]
                         , body :: [Expression] } deriving (Eq, Show)

type Pasha = ErrorT String (StateT Program IO)
