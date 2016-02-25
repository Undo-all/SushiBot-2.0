{-# LANGUAGE OverloadedStrings #-}

module Bot
( Bot(..)
, Command(..)
, Pattern(..)
, Special(..)
, RequestInfo(..)
, makeBot
, say
, privmsg
, privmsg'
, act
, privact
) where

import Network
import Data.Char
import Data.List
import System.IO
import Data.IORef
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.DeepSeq
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Database.SQLite.Simple
import Data.Text.Lazy.Builder
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as T
import Control.Concurrent.Chan.Unagi
import qualified Data.Map.Strict as M
import Control.Concurrent hiding (newChan, readChan, writeChan)

data Msg = Ping Text
         | PM Text PrivMsg
         deriving (Eq, Show)

data PrivMsg = PrivMsg Text Text
             | Call Text Text [Text]
             deriving (Eq, Show)

parseMsg :: Text -> Maybe Msg
parseMsg xs
    | T.take 4 xs == "PING" = Just $ Ping (T.drop 6 xs)
    | otherwise             = parsePM xs 

parsePM :: Text -> Maybe Msg
parsePM xs = uncurry PM . fmap parseCall <$> parsePrivMsg xs
  where parseCall (PrivMsg user xs)
            | isCommand xs = let (comm:args) = getArgs (T.tail xs)
                             in Call user comm args
            | otherwise    = PrivMsg user xs
        isCommand xs =
            T.head xs == '!' && T.length xs >= 2 && xs `T.index` 2 /= '!'

getArgs :: Text -> [Text]
getArgs = getArgs' mempty [] False
{-# INLINE getArgs #-}

append :: Builder -> [Text] -> [Text]
append tmp res = let xs = toStrict (toLazyText (flush <> tmp))
                 in if T.null xs then res else xs:res
{-# INLINE append #-}

getArgs' :: Builder -> [Text] -> Bool -> Text -> [Text]
getArgs' tmp res _ txt
    | T.null txt = reverse $ append tmp res

getArgs' tmp res True txt
    | T.head txt == '\\' =
      if T.length txt >= 2 && T.head (T.tail txt) == '"'
        then getArgs' (tmp <> singleton '"') res True (T.tail txt)
        else getArgs' (tmp <> singleton '\\') res True (T.tail txt)
    | T.head txt == '"' = getArgs' mempty (append tmp res) False (T.tail txt)
    | otherwise         =
      getArgs' (tmp <> singleton (T.head txt)) res True (T.tail txt)

getArgs' tmp res False txt
    | T.head txt == '"'    = getArgs' mempty (append tmp res) True (T.tail txt)
    | isSpace (T.head txt) =
      getArgs' mempty (append tmp res) False (T.tail txt)
    | otherwise            =
      getArgs' (tmp <> singleton (T.head txt)) res False (T.tail txt)

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
    in pure (T.toLower chan, PrivMsg name msg)

data Bot = Bot
         { botName     :: Text
         , botHandle   :: Handle
         , botChannels :: IORef (Map Text (ThreadId, InChan PrivMsg))
         , botCommands :: Map Text Command
         , botPatterns :: [Pattern]
         , botDbConn   :: Connection
         } 

data RequestInfo = RequestInfo
                 { reqChan   :: Text
                 , reqUser   :: Text
                 , reqHandle :: Handle
                 , reqDbConn :: Connection
                 }

data Command = Command
             { commandDesc   :: Text
             , commandSyntax :: Text
             , commandArgs   :: (Int, Maybe Int)
             , commandFunc   :: [Text] -> ReaderT RequestInfo IO ()
             }

instance NFData Command where
    rnf (Command d s a f) = rnf d `seq` rnf s `seq` rnf a `seq` rnf f `seq` ()

type Pattern = Text -> ReaderT RequestInfo IO ()
type Special = Bot -> Text -> IO ()

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
privact chan xs = do
    h <- asks reqHandle
    liftIO $ privmsg' h chan $ T.concat ["\0001ACTION ", xs, "\0001"]

act :: Text -> ReaderT RequestInfo IO ()
act xs = do
    chan <- asks reqChan
    privact chan xs

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

