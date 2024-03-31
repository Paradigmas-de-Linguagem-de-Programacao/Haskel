module Fliperama.DataTypes.Session (Session(..)) where

data Session = Session { ownerUserName :: String, menuStateMessage :: String } deriving (Show)