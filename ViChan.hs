{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ViChan where

import Data.Ord
import Data.List
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Trans
import Data.Vector (Vector)
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Control.Concurrent.Async.Lifted
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.HashMap.Strict as HM

data Post = Post
          { postNumber :: {-# UNPACK #-} !Int
          , postSubject :: !(Maybe Text)
          , postStickied :: !Bool
          , postTime :: {-# UNPACK #-} !Int
{--
          , postComment :: !(Maybe Text)
          , postEmail :: !(Maybe Text)
          , postName :: !(Maybe Text)
          , postCapcode :: !(Maybe Text)
          , postTime :: {-# UNPACK #-} !Int
          , postOmittedPosts :: !(Maybe Int)
          , postOmittedImages :: !(Maybe Int)
          , postReplies :: !(Maybe Int)
          , postImages :: !(Maybe Int)
          , postStickied :: {-# UNPACK #-} !Bool
          , postLocked :: {-# UNPACK #-} !Bool
          , postLastModified :: {-# UNPACK #-} !Int
          , postThumbnailHeight :: !(Maybe Int)
          , postThumbnailWidth :: !(Maybe Int)
          , postImageHeight :: !(Maybe Int)
          , postFileSize :: !(Maybe Int)
          , postFileName :: !(Maybe Text)
          , postFileExtension :: !(Maybe Text)
          , postMD5 :: !(Maybe Text)
          , postReplies :: !(Maybe Int) 
--}
          } deriving (Generic, Show)

instance FromJSON Post where
    parseJSON (Object v) =
        Post <$> v .: "no"
             <*> v .:? "sub"
             <*> (maybe False toEnum <$> v .:? "sticky")
             <*> v .: "time"
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
  where notStickied Post { postStickied = stickied } = not stickied

getLastPost :: Text -> Text -> MaybeT IO (Post, Post)
getLastPost site board = do
    op <- getLastBumped site board
    reply <- V.last <$> getPosts site board (postNumber op)
    return (op, reply)

acrossBoards :: (Text -> Text -> MaybeT IO a) -> Text -> [Text] -> MaybeT IO [(Text, a)]
acrossBoards f site boards = do
    asyncs <- mapM async (map (\x -> fmap ((,) x) (f site x)) boards)
    xs <- mapM wait asyncs
    return xs

getRecentThread :: Text -> [Text] -> MaybeT IO (Text, Post)
getRecentThread site boards =
    maximumBy (comparing (postTime . snd)) <$>
    acrossBoards getLastBumped site boards

getRecentPost :: Text -> [Text] -> MaybeT IO (Text, Post, Post)
getRecentPost site boards = 
    maximumBy (comparing (postTime . thrd)) . map (\(x,(y,z)) -> (x,y,z)) <$>
    acrossBoards getLastPost site boards
  where thrd (x,y,z) = z

makeThreadUrl :: Text -> (Text, Post) -> Text
makeThreadUrl site (board, op) =
    T.concat [ site, "/", board, "/res/"
             , T.pack (show (postNumber op)), ".html"
             ]

makePostUrl :: Text -> (Text, Post, Post) -> Text
makePostUrl site (board, op, post)
    | postNumber op == postNumber post = makeThreadUrl site (board, op)
    | otherwise =
      T.concat [ makeThreadUrl site (board, op), "#"
               , T.pack (show (postNumber post))
               ]

