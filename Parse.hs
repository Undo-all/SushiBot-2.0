{-# LANGUAGE OverloadedStrings #-}

module Parse (parseMsg) where

import Msg
import Data.Char
import Data.Monoid
import Control.Monad
import Data.Text (Text)
import Data.Text.Lazy.Builder
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

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

append :: Builder -> [Text] -> [Text]
append tmp res = let xs = toStrict (toLazyText (flush <> tmp))
                 in if T.null xs then res else xs:res

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

