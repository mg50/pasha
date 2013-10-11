{-# LANGUAGE GADTs #-}
module Types where

data Program = Program [Function]

data Expression = Variable String
                | StringLit String
                | Assignment String Expression
                | FunctionCall String [Expression] deriving (Eq, Show)

data Function = Function { funcName :: String
                         , funcParams :: [String]
                         , body :: [Expression] } deriving (Eq, Show)
