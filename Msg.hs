module Msg where

import Data.Text (Text)

-- TODO: Add more Msg types, make Pattern in module Types be of the type
-- Bot -> Msg -> IO () and remove Special type.
data Msg = Ping Text
         | PM Text PrivMsg
         deriving (Eq, Show)

data PrivMsg = PrivMsg Text Text
             | Call Text Text [Text] 
             deriving (Eq, Show)

