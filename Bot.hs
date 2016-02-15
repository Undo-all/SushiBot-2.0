{-# LANGUAGE OverloadedStrings #-}

module Bot
( Bot(..)
, Command(..)
, Pattern(..)
, RequestInfo(..)
, makeBot
, say
, privmsg
, act
, privact
) where

import Network
import Data.Char
import Data.List
import System.IO
import Data.IORef
import Data.Maybe
import Control.Monad
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent.Chan.Unagi
import qualified Data.Map.Strict as M
import Control.Concurrent hiding (newChan, readChan, writeChan)

data Msg = Ping Text
         | Join Text Text
         | PM Text PrivMsg
         deriving (Eq, Show)

data PrivMsg = PrivMsg Text Text
             | Call Text Text [Text]
             deriving (Eq, Show)

parseMsg :: Text -> Maybe Msg
parseMsg xs
    | T.length xs < 6       = Nothing
    | T.take 4 xs == "PING" = Just $ Ping (T.drop 6 xs)
    | otherwise             = parsePM xs <|> parseJoin xs

parsePM :: Text -> Maybe Msg
parsePM xs = uncurry PM . fmap parseCall <$> parsePrivMsg xs
  where parseCall (PrivMsg user xs)
            | isCommand xs = let (comm:args) = getArgs (T.tail xs)
                             in Call user comm args
            | otherwise    = PrivMsg user xs
        isCommand xs =
            T.head xs == '!' && T.length xs >= 2 && xs `T.index` 2 /= '!'

getArgs :: Text -> [Text]
getArgs = getArgs' [] [] False . T.unpack

getArgs' :: String -> [Text] -> Bool -> String -> [Text]
getArgs' tmp res _ []
    | null tmp  = reverse res
    | otherwise = reverse (T.pack (reverse tmp):res)
getArgs' tmp res True ('\\':'"':xs) = getArgs' ('"':tmp) res True xs
getArgs' tmp res True ('"':xs)      =
    getArgs' [] (T.pack (reverse tmp):res) False xs
getArgs' tmp res True (c:xs)        = getArgs' (c:tmp) res True xs
getArgs' tmp res False ('"':xs)
    | null tmp  = getArgs' [] res True xs
    | otherwise = getArgs' [] (T.pack (reverse tmp):res) True xs
getArgs' tmp res False (c:xs) 
    | isSpace c =
      if null tmp
        then getArgs' [] res False xs 
        else getArgs' [] (T.pack (reverse tmp):res) False xs
    | otherwise = getArgs' (c:tmp) res False xs

-- Pls let ApplicativeDo come out soon
parsePrivMsg :: Text -> Maybe (Text, PrivMsg)
parsePrivMsg t0 = 
    guard (T.head t0 == ':') *>
    let (name, t1) = T.break (=='!') (T.tail t0)
    in guard (not $ T.null t1) *>
    let t2 = T.dropWhile (/=' ') t1
    in guard (T.length t2 >= 8) *>
    let (priv, t3) = T.splitAt 8 (T.tail t2)
    in guard (priv == "PRIVMSG ") *>
    let (chan, t4) = T.break (==' ') t3
        t5         = T.drop 2 t4
        msg        = T.takeWhile (/='\r') t5
    in pure (chan, PrivMsg name msg)

parseJoin :: Text -> Maybe Msg
parseJoin t0 =
    guard (T.head t0 == ':') *>
    let (name, t1) = T.break (=='!') (T.tail t0)
    in guard (not $ T.null t1) *>
    let t2 = T.dropWhile (/=' ') t1
    in guard (T.length t2 >= 5) *>
    let (xs, t3) = T.splitAt 5 (T.tail t2)
    in guard (xs == "JOIN") *>
    let chan = T.takeWhile (/='\r') $ T.drop 2 t3
    in pure (Join name chan)

data Bot = Bot
         { botName     :: Text
         , botHandle   :: Handle
         , botChannels :: IORef (Map Text (ThreadId, InChan PrivMsg))
         , botThread   :: ThreadId
         , botCommands :: Map Text Command
         , botPatterns :: [Pattern]
         , botTells    :: IORef (Map (Text, Text) Text)
         } 

data RequestInfo = RequestInfo
                 { reqChan   :: Text
                 , reqUser   :: Text
                 , reqHandle :: Handle
                 }

data Command = Command
             { commandDesc   :: Text
             , commandSyntax :: Text
             , commandArgs   :: (Int, Maybe Int)
             , commandFunc   :: [Text] -> ReaderT RequestInfo IO ()
             }

type Pattern = Text -> ReaderT RequestInfo IO ()

privmsg' :: Handle -> Text -> Text -> IO ()
privmsg' h chan xs = T.hPutStrLn h $ T.concat ["PRIVMSG ", chan, " :", xs]

privmsg :: Text -> Text -> ReaderT RequestInfo IO ()
privmsg chan xs = do
    h <- asks reqHandle
    liftIO $ privmsg' h chan xs

say :: Text -> ReaderT RequestInfo IO ()
say xs = do
    chan <- asks reqChan
    privmsg chan xs

privact :: Text -> Text -> ReaderT RequestInfo IO ()
privact chan xs = privmsg chan $ T.concat ["\0001ACTION ", xs, "\0001"]

act :: Text -> ReaderT RequestInfo IO ()
act xs = do
    chan <- asks reqChan
    privact chan xs

makeBot :: Text -> [Text] -> Map Text Command -> [Pattern] -> HostName -> Int -> IO Bot
makeBot name chans comms patterns host port = do
    h    <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering h LineBuffering
    ref1 <- newIORef (M.fromList [])
    ref2 <- newIORef (M.fromList [])
    wait <- newEmptyMVar
    n    <- forkIO $ mainLoop wait h ref1 ref2
    T.hPutStrLn h $ T.concat ["USER ", name, " ", name, " ", name, " :", name]
    T.hPutStrLn h $ T.concat ["NICK ", name]
    let bot = Bot name h ref1 n comms patterns ref2
    readMVar wait
    mapM_ (joinChan bot) chans 
    return bot

mainLoop :: MVar () -> Handle -> IORef (Map Text (ThreadId, InChan PrivMsg)) -> IORef (Map (Text, Text) Text) -> IO ()
mainLoop wait h ref1 ref2 = forever $ do
    chans <- readIORef ref1
    tells <- readIORef ref2
    xs    <- T.hGetLine h
    T.putStrLn xs
    case parseMsg xs of
        Just (Ping xs)    -> do
            T.hPutStrLn h $ T.concat ["PONG :", xs]
            tryPutMVar wait ()
            return ()
        Just (PM chan pm) ->
            case M.lookup chan chans of
                Just (_, inchan) -> writeChan inchan pm 
                Nothing          -> return ()
        Just (Join name chan) ->
            case M.lookup (name, chan) tells of
                Just msg -> do privmsg' h chan msg
                Nothing  -> return ()
        Nothing           -> return ()

joinChan :: Bot -> Text -> IO ()
joinChan bot@(Bot _ h chans _ _ _ _) chan = do
    T.hPutStrLn h $ T.concat ["JOIN ", chan]
    (inchan, outchan) <- newChan
    n                 <- forkIO $ handleChan bot outchan chan 
    atomicModifyIORef' chans (\m -> (M.insert chan (n, inchan) m, ()))

userInChannel :: Handle -> Text -> Text -> IO Bool
userInChannel h chan name = do
    T.hPutStrLn h $ T.concat ["NAMES ", chan]
    names <- T.words <$> T.hGetLine h
    return . isJust $ find (\x -> x == name || x == T.tail name) names

handleChan :: Bot -> OutChan PrivMsg -> Text -> IO ()
handleChan bot outchan chan = forever $ do
    msg <- readChan outchan
    case msg of
        Call user "tell" xs -> do
            let to  = head xs
                msg = T.unwords (tail xs)
            n <- userInChannel (botHandle bot) chan to
            if n 
              then atomicModifyIORef' (botTells bot) 
                                      (\x -> (M.insert (to, chan) msg x, ()))
              else privmsg' (botHandle bot) to msg
        Call usr comm args -> call bot usr chan comm args
        PrivMsg usr xs     -> runReaderT (mapM_ ($ xs) (botPatterns bot))
                                         (RequestInfo chan usr (botHandle bot))

call :: Bot -> Text -> Text -> Text -> [Text] -> IO ()
call bot usr chan comm args =
    case M.lookup comm (botCommands bot) of
        Nothing                      ->
            privmsg' (botHandle bot) chan $ T.append "Command not found: " comm
        Just (Command _ _ numArgs f) -> do
            case checkNumArgs numArgs (length args) of
              Nothing  -> 
                  runReaderT (f args) (RequestInfo chan usr (botHandle bot))
              Just err -> privmsg' (botHandle bot) chan $  
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

