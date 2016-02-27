module Types where

import Msg
import Control.DeepSeq
import Data.Text (Text)
import Data.IORef (IORef)
import System.IO (Handle)
import Data.Map.Strict (Map)
import Control.Concurrent (ThreadId)
import Control.Monad.Reader (ReaderT)
import Database.SQLite.Simple (Connection)
import Control.Concurrent.Chan.Unagi (InChan)

data Bot = Bot
         { botName     :: Text
         , botHandle   :: Handle
         , botChannels :: IORef (Map Text (ThreadId, InChan PrivMsg))
         , botCommands :: Map Text Command
         , botPatterns :: [Pattern]
         , botDbConn   :: Connection
         } 

data Command = Command
             { commandDesc   :: Text
             , commandSyntax :: Text
             , commandArgs   :: (Int, Maybe Int)
             , commandFunc   :: [Text] -> ReaderT RequestInfo IO ()
             }

data RequestInfo = RequestInfo
                 { reqChan   :: Text
                 , reqUser   :: Text
                 , reqHandle :: Handle
                 , reqDbConn :: Connection
                 }

instance NFData Command where
    rnf (Command d s a f) = rnf d `seq` rnf s `seq` rnf a `seq` rnf f `seq` ()

type Pattern = Text -> ReaderT RequestInfo IO ()
type Special = Bot -> Text -> IO ()

