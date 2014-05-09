{-# LANGUAGE GADTs #-}
module Types where
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.Map as M

type Program = [Function]

data Expression = Variable String
                | StringLit { fromStringLit :: String }
                | Assignment String Expression
                | FunctionCall String [Expression] deriving (Eq, Show)

data Function = Function { funcName :: String
                         , funcParams :: [String]
                         , body :: [Expression] } deriving (Eq, Show)

data AWSMode = Sandbox | Production deriving (Eq)

data Config = Config { accessKey :: String
                     , awsSecret :: String
                     , reward :: Float
                     , lifetime :: Int
                     , program :: Program
                     , duration :: Int
                     , awsMode :: AWSMode }

type Bindings = M.Map String String

type Pasha = StateT Bindings (ReaderT Config IO)

runPasha :: Config -> Pasha a -> IO a
runPasha conf pasha = runReaderT (evalStateT pasha M.empty) conf
