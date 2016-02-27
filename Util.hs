{-# LANGUAGE OverloadedStrings #-}

module Util where

import Types
import System.IO
import Data.Text (Text)
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T

privmsg' :: Handle -> Text -> Text -> IO ()
privmsg' h chan xs = T.hPutStrLn h $ T.concat ["PRIVMSG ", chan, " :", xs]

privmsgs' :: Handle -> Text -> [Text] -> IO ()
privmsgs' h chan xs =
    let pms = map (\x -> T.concat ["PRIVMSG ", chan, " :", x, "\r"]) xs
    in T.hPutStr h (T.concat pms) *> hFlush h

privmsg :: Text -> Text -> ReaderT RequestInfo IO ()
privmsg chan xs = do
    h <- asks reqHandle
    liftIO $ privmsg' h chan xs

privmsgs :: Text -> [Text] -> ReaderT RequestInfo IO ()
privmsgs chan xs = do
    h <- asks reqHandle
    liftIO $ privmsgs' h chan xs

say :: Text -> ReaderT RequestInfo IO ()
say xs = do
    chan <- asks reqChan
    privmsg chan xs

sayList :: [Text] -> ReaderT RequestInfo IO ()
sayList xs = do
    chan <- asks reqChan
    privmsgs chan xs

privact :: Text -> Text -> ReaderT RequestInfo IO ()
privact chan xs = do
    h <- asks reqHandle
    liftIO $ privmsg' h chan $ T.concat ["\0001ACTION ", xs, "\0001"]

act :: Text -> ReaderT RequestInfo IO ()
act xs = do
    chan <- asks reqChan
    privact chan xs

