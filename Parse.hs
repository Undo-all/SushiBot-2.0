{-# LANGUAGE OverloadedStrings #-}

module Parse (parseMsg) where

import Msg
import Data.Char
import Data.Monoid
import Control.Monad
import Data.Text (Text)
import Control.Applicative
import Data.Text.Lazy.Builder
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

-- All the parsing here is ugly and applicative for performance reasons.
-- It'll get clean when ApplicativeDo comes out though.

parseMsg :: Text -> Maybe Msg
parseMsg xs
    | T.take 4 xs == "PING" = Just (Ping (T.drop 6 xs))
    | otherwise =
      guard (T.head xs == ':') *>
      let (name, t1) = T.break (=='!') (T.tail xs)
          t2         = T.dropWhile (/=' ') t1
      in guard (not (T.null t2)) *>
      let t3 = T.tail t2
      in parseJoin name t3 <|> parsePM name t3

parseJoin :: Text -> Text -> Maybe Msg
parseJoin name t0 =
    guard (T.length t0 >= 6) *>
    let (join, t1) = T.splitAt 6 t0
    in guard (join == "JOIN :") *>
    let chan = T.takeWhile (/='\r') t1
    in pure (Join name chan)

parsePM :: Text -> Text -> Maybe Msg
parsePM name t0 = uncurry PM . fmap parseCall <$> parsePrivMsg name t0
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
    | T.null txt = reverse (append tmp res)

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

parsePrivMsg :: Text -> Text -> Maybe (Text, PrivMsg)
parsePrivMsg name t0 = 
    guard (T.length t0 >= 8) *>
    let (priv, t1) = T.splitAt 8 t0
    in guard (priv == "PRIVMSG ") *>
    let (chan, t2) = T.break (==' ') t1
        t3         = T.drop 2 t2
        msg        = T.takeWhile (/='\r') t3
    in pure (T.toLower chan, PrivMsg name msg)

