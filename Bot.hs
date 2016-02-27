{-# LANGUAGE OverloadedStrings #-}

module Bot
( Bot(..)
, Command(..)
, Pattern(..)
, Special(..)
, RequestInfo(..)
, makeBot
, say
, sayList
, privmsg
, privmsgs
, privmsg'
, privmsgs'
, act
, privact
) where

import Msg
import Util
import Parse
import Types
import Network
import System.IO
import Data.IORef
import Control.DeepSeq
import Data.Text (Text)
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent.Chan.Unagi
import Database.SQLite.Simple (open)
import qualified Data.Map.Strict as M
import Control.Concurrent hiding (newChan, readChan, writeChan)

makeBot :: Text -> [Text] -> Map Text Command -> [Pattern] -> [Special] -> HostName -> Int -> String -> IO Bot
makeBot name chans comms patterns specials host port db = do
    h    <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering h LineBuffering
    ref  <- newIORef (M.fromList [])
    wait <- newEmptyMVar
    conn <- open db
    let bot = comms `deepseq` Bot name h ref comms patterns conn
    forkIO $ mainLoop wait specials bot
    T.hPutStrLn h $ T.concat ["USER ", name, " ", name, " ", name, " :", name]
    T.hPutStrLn h $ T.concat ["NICK ", name]
    readMVar wait
    mapM_ (joinChan bot) chans 
    return bot

mainLoop :: MVar () -> [Special] -> Bot -> IO ()
mainLoop wait specials bot@(Bot { botHandle = h, botChannels = ref }) = forever $ do
    chans <- readIORef ref
    xs    <- T.hGetLine h
    mapM_ (\s -> s bot xs) specials
    case parseMsg xs of
        Just (Ping xs) -> do
            T.hPutStrLn h $ T.concat ["PONG :", xs]
            tryPutMVar wait ()
            return ()

        Just (PM chan pm) ->
            case M.lookup chan chans of
                Just (_, inchan) -> writeChan inchan pm 
                Nothing          -> return ()

        Nothing -> return ()

joinChan :: Bot -> Text -> IO ()
joinChan bot@(Bot { botHandle = h, botChannels = chans }) chan = do
    T.hPutStrLn h $ T.concat ["JOIN ", chan]
    (inchan, outchan) <- newChan
    n                 <- forkIO $ handleChan bot outchan chan 
    atomicModifyIORef' chans (\m -> (M.insert chan (n, inchan) m, ()))

handleChan :: Bot -> OutChan PrivMsg -> Text -> IO ()
handleChan bot@(Bot { botHandle = h, botDbConn = conn }) outchan chan = forever $ do
    msg <- readChan outchan
    case msg of
        Call usr comm args -> call bot usr chan comm args
        PrivMsg usr xs     -> runReaderT (mapM_ ($ xs) (botPatterns bot))
                                         (RequestInfo chan usr h conn)

call :: Bot -> Text -> Text -> Text -> [Text] -> IO ()
call bot@(Bot { botHandle = h, botDbConn = conn }) usr chan comm args =
    case M.lookup comm (botCommands bot) of
        Nothing                      ->
            privmsg' h chan $ T.append "Command not found: " comm
        Just (Command _ _ numArgs f) -> 
            case checkNumArgs numArgs (length args) of
              Nothing  -> 
                  runReaderT (f args) (RequestInfo chan usr h conn)
              Just err -> privmsg' h chan $  
                  T.concat [ "Incorrect number of arguments to command "
                           , comm, " (expected ", err, ", got "
                           , T.pack . show . length $ args, ")"
                           ]

checkNumArgs :: (Int, Maybe Int) -> Int -> Maybe Text
checkNumArgs (x, Just y) n
    | n >= x && n <= y = Nothing
    | otherwise        =
      Just . T.pack $ "between " ++ show x ++ " and " ++ show y ++ " arguments"
checkNumArgs (x, Nothing) n
    | n >= x    = Nothing
    | otherwise =
      Just . T.pack $ "greater than " ++ show x ++ " argument" ++ 
      if x == 1 then "" else "s"

