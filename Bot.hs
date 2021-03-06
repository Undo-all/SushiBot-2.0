{-# LANGUAGE OverloadedStrings #-}

module Bot
( Bot(..)
, Command(..)
, Msg(..)
, CustomHandler
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

makeBot :: Text -> [Text] -> Map Text Command -> CustomHandler -> HostName -> Int -> String -> IO Bot
makeBot name chans comms patterns host port db = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h LineBuffering -- Line buffering is most efficient.

    ref  <- newIORef (M.fromList [])
    conn <- open db 
    -- This MVar exists to block from joining the channels until we've
    -- responded to at least one PING.
    wait <- newEmptyMVar 
    
    -- The commands are deepseq'd here so that there isn't a noticable
    -- delay when using commands for the first time in a connection.
    let bot = comms `deepseq` Bot name h ref comms patterns conn
    forkIO (mainLoop wait bot)

    T.hPutStrLn h (T.concat ["USER ", name, " ", name, " ", name, " :", name])
    T.hPutStrLn h (T.concat ["NICK ", name])
    
    -- Block until wait is full.
    readMVar wait
    mapM_ (joinChan bot) chans 
    return bot

joinChan :: Bot -> Text -> IO ()
joinChan bot@Bot{ botHandle = h, botChannels = chans } chan = do
    T.hPutStrLn h (T.concat ["JOIN ", chan])
    (inchan, outchan) <- newChan
    n <- forkIO (handleChan bot outchan chan)
    atomicModifyIORef' chans (\m -> (M.insert chan (n, inchan) m, ()))

mainLoop :: MVar () -> Bot -> IO ()
mainLoop wait bot@Bot{ botHandle = h } = forever $ do
    msg <- parseMsg <$> T.hGetLine h 
    case msg of
        Just msg -> do botCustomHandler bot bot msg
                       handleMsg wait bot msg
        Nothing  -> return ()  

handleMsg :: MVar () -> Bot -> Msg -> IO ()
handleMsg wait bot msg = case msg of
    Ping xs -> do
        T.hPutStrLn (botHandle bot) (T.concat ["PONG :", xs])
        tryPutMVar wait () -- Fills 'wait' MVar, unblocking JOIN commands.
        return ()

    PM chan pm -> do
        chans <- readIORef (botChannels bot)
        case M.lookup chan chans of
            -- We redirect the PrivMsg to the appropriate channel handler.
            Just (_, inchan) -> writeChan inchan pm 
            Nothing          -> return ()

    _ -> return ()

handleChan :: Bot -> OutChan PrivMsg -> Text -> IO ()
handleChan bot@Bot{ botHandle = h, botDbConn = conn } outchan chan = forever $ do
    msg <- readChan outchan
    case msg of
        Call usr comm args -> call bot usr chan comm args
        PrivMsg usr xs -> return ()

call :: Bot -> Text -> Text -> Text -> [Text] -> IO ()
call bot@Bot{ botHandle = h, botDbConn = conn } usr chan comm args =
    case M.lookup comm (botCommands bot) of
        Nothing ->
            privmsg' h chan (T.append "Command not found: " comm)
        Just (Command _ _ numArgs f) -> 
            case checkNumArgs numArgs (length args) of
                Nothing  -> 
                    runReaderT (f args) (RequestInfo chan usr h conn)
                Just err -> privmsg' h chan (errMsg comm err)
  where errMsg comm err = 
            T.concat [ "Incorrect number of arguments to command "
                     , comm, " (expected ", err, ", got "
                     , T.pack (show (length args)), ")"
                     ]

checkNumArgs :: (Int, Maybe Int) -> Int -> Maybe Text
checkNumArgs (x, Just y) n
    | n >= x && n <= y = Nothing
    | otherwise        =
      Just (T.pack errMsg)
  where errMsg | x == 0 && y == 0 = "no arguments"
               | x == y           = sayNumArgs x
               | otherwise        =
                 "between " ++ show x ++ " and " ++ show y ++ " arguments"

checkNumArgs (x, Nothing) n
    | n >= x    = Nothing
    | otherwise =
      Just (T.pack ("greater than " ++ sayNumArgs x))

sayNumArgs :: Int -> String
sayNumArgs x = show x ++ " argument" ++ if x == 1 then "" else "s"

