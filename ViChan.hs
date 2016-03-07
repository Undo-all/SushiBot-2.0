{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ViChan (getRecent, getRecentBoard) where

import Data.Char
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad.Trans
import Data.Vector (Vector)
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.HashMap.Strict as HM

data Post = Post
          { postNumber :: {-# UNPACK #-} !Int
          , postStickied :: {-# UNPACK #-} !Int
          } deriving (Generic, Show)

instance FromJSON Post where
    parseJSON (Object v) =
        Post <$> v .: "no"
             <*> (fromMaybe 0 <$> v .:? "sticky")
    parseJSON _ = mempty

getThreads :: Text -> Text -> MaybeT IO (Vector Post)
getThreads site board = do
    let url = T.concat [site, "/", board, "/catalog.json"]
    content <- MaybeT (decode <$> simpleHttp (T.unpack url))
    let (Array v) = content
        (Object hm) = V.head v
        (Just (Array xs)) = HM.lookup "threads" hm
    case V.mapM fromJSON xs of
        Success threads -> return threads
        Error _ -> empty

getPosts :: Text -> Text -> Int -> MaybeT IO (Vector Post)
getPosts site board no = do
    let url = T.concat [site, "/", board, "/res/", T.pack (show no), ".json"]
    content <- MaybeT (decode <$> simpleHttp (T.unpack url) :: IO (Maybe Value))
    let (Object hm) = content
        (Just (Array xs)) = HM.lookup "posts" hm
    case V.mapM fromJSON xs of
        Success posts -> return posts
        Error _ -> empty

getLastBumped :: Text -> Text -> MaybeT IO Post
getLastBumped site board =do
    xs <- getThreads site board
    return (V.head (V.filter notStickied xs))
  where notStickied Post { postStickied = stickied } = stickied == 0

getRecentBoard :: Text -> Text -> MaybeT IO Text
getRecentBoard site board = do
    op <- getLastBumped site board
    reply <- V.last <$> getPosts site board (postNumber op)
    return (makePostUrl site board op reply)

getRecent :: Text -> IO Text
getRecent site = do
    let url = T.concat [site, "/recent.html"]
    xs <- toStrict <$> simpleHttp (T.unpack url)
    let b0 = BS.drop 3147 xs
        b1 = BS.dropWhile (/=quote) b0
        b2 = BS.tail b1
        b3 = BS.takeWhile (/=quote) b2
    return (site `T.append` decodeUtf8 b3)
  where quote = fromIntegral (ord '"')

makeThreadUrl :: Text -> Text -> Post -> Text
makeThreadUrl site board post =
    T.concat [ site, "/", board, "/res/"
             , T.pack (show (postNumber post)), ".html"
             ]

makePostUrl :: Text -> Text -> Post -> Post -> Text
makePostUrl site board op post
    | postNumber op == postNumber post = makeThreadUrl site board op
    | otherwise =
      T.concat [ makeThreadUrl site board op, "#"
               , T.pack (show (postNumber post))
               ]

