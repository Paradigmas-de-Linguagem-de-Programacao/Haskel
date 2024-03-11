module Models.User (User(..)) where

data User = User 
    { username :: String
    , password :: String
    } deriving (Show)