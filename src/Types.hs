{-# LANGUAGE GADTs #-}
module Types where
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

type Program = [Function]

data Expression = Variable String
                | StringLit { fromStringLit :: String }
                | Assignment String Expression
                | FunctionCall String [Expression] deriving (Eq, Show)

data Function = Function { funcName :: String
                         , funcParams :: [String]
                         , body :: [Expression] } deriving (Eq, Show)

type Bindings = [(String, String)]
data Env = Env { fns :: Program
               , bindings :: Bindings }

type Pasha = StateT Bindings (ReaderT Program IO)
