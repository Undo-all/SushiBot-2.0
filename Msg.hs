module Msg where

import Data.Text (Text)

data Msg = Ping Text
         | PM Text PrivMsg
         deriving (Eq, Show)

data PrivMsg = PrivMsg Text Text
             | Call Text Text [Text]
             deriving (Eq, Show)

