{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot
import Data.Char
import Data.Maybe
import Control.Monad
import System.Random
import System.Process
import Data.Text (Text)
import Text.HTML.Scalpel
import Control.Concurrent
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Database.SQLite.Simple
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M

listItems :: [Text] -> Text
listItems [x]    = x
listItems [x, y] = T.concat [x, " and ", y]
listItems (x:xs) = T.concat [x, ", ", listItems xs]

choice :: [a] -> IO a
choice xs = (xs !!) <$> randomRIO (0, length xs - 1)

specialJoin :: Special
specialJoin Bot { botHandle = h, botDbConn = conn } xs = 
    case parseJoin xs of
        Just name -> do
            msg <- runMaybeT (getTold name)
            maybe (return ()) (privmsg' h name) msg
        Nothing   -> return ()
  where parseJoin t0 =
            guard (T.head t0 == ':') *>
            let (name, t1) = T.break (=='!') (T.tail t0)
            in guard (not $ T.null t1) *>
            let t2 = T.dropWhile (/=' ') t1
            in guard (T.length t2 >= 5) *>
            let xs = T.take 5 (T.tail t2)
            in guard (xs == "JOIN ") *>
            pure name
        getTold name = do
            [Only userExists] <- liftIO (query conn queryExists (Only name))
            guard userExists
            [Only hasTold] <- liftIO (query conn queryHasTold (Only name))
            guard hasTold
            liftIO $ do
                [(from, msg)] <- query conn queryTold (Only name)
                execute conn queryRemove (Only name)
                return (T.concat [from, " wanted me to tell you: ", msg])
        queryExists  = "SELECT (count(*) > 0) FROM users WHERE user_name = ?"
        queryHasTold = "SELECT user_has_told FROM users WHERE user_name = ?"
        queryTold    = "SELECT user_told_from, user_told_msg FROM users \
                       \WHERE user_name = ?"
        queryRemove  = "UPDATE users SET user_has_told = 0,\
                       \user_told_from = NULL, user_told_msg = NULL \
                       \WHERE user_name = ?"

commandInfo :: Command
commandInfo =
    Command
        "Show general info about SushiBot."
        ""
        (0, Just 0)
        (\_ -> say info)
  where info = "SushiBot is a bot written in Haskell by the god-like being \
               \that is undoall. It's pretty shit"

commandHelp :: Command
commandHelp =
    Command
        "List availible commands, or get help with a specific command."
        "(command)"
        (0, Just 1)
        help
  where help [c] = case M.lookup c commands of
            Just comm -> do
                say (commandDesc comm)
                say (T.concat ["Syntax: !", c, " ", commandSyntax comm])
            Nothing -> say $ "Command not found: " `T.append` c

        help [] = do
            usr <- asks reqUser
            say "List of commands sent in a PM."
            privmsgs usr descriptions

        descriptions = zipWith (\x y -> T.concat [x, " - ", y])
                               (M.keys commands)
                               (map commandDesc (M.elems commands))

commandSay :: Command
commandSay =
    Command
        "Say something."
        "[sentence]"
        (1, Nothing)
        (say . T.unwords)

commandAct :: Command
commandAct =
    Command
        "Do an action."
        "[action]"
        (1, Nothing)
        (act . T.unwords)

commandSource :: Command
commandSource =
    Command
        "Get a link to the source code."
        ""
        (0, Just 0)
        (\_ -> say source)
  where source = "The source can be found at \
                 \https://github.com/Undo-all/SushiBot-2.0"

commandSlap :: Command
commandSlap =
    Command
        "Slap someone (or multiple people) with a large trout"
        "[user | users]"
        (1, Nothing)
        (act . slap . listItems)
  where slap xs = T.concat ["slaps ", xs, " with a large trout."]

commandMix :: Command
commandMix =
    Command
        "Mix two or more drinks."
        "[drinks]"
        (2, Nothing)
        mix
  where mix xs = do
            usr <- asks reqUser
            act $ T.concat [ "skillfully mixes ", listItems xs
                           , " and slides it to ", usr
                           ]

commandKill :: Command
commandKill =
    Command
        "Kill someone!"
        "[user]"
        (1, Just 1)
        kill
  where kill [n] 
            | n `elem` fuckYous = say "Fuck you too, buddy."
            | otherwise = do
                usr <- asks reqUser
                if n == usr || n == "me" || n == "us"
                  then say suicide
                  else say (kills n)

        fuckYous = ["SushiBot", "myself", "hisself", "herself", "itself"
                   , "his self", "her self", "its self", "yourself", "bot"
                   , "it self", "xemself", "bots", "sushi", "undoall"
                   , "thyself", "thy self", "all bots", "sushi bot"
                   , "themselves", "them selves", "themself", "them self"
                   ]

        kills n  = T.concat [ "If a shitty IRC bot could kill ", n
                            , ", than someone would have done it already."
                            ]

        suicide  = "I would link to a suicide hotline, but considering the \
                   \fact that you're trying to use an IRC bot to kill \
                   \yourself, I'm not too worried."

commandFortune :: Command
commandFortune =
    Command
        "A direct call to the unix command 'fortune'."
        ""
        (0, Just 0)
        (\_ -> liftIO readFortune >>= sayList . T.lines . T.pack . init)
  where readFortune = readProcess "fortune" [] []

commandLewd :: Command
commandLewd =
    Command
        "Lewd a senpai :3"
        "[user]"
        (1, Just 1)
        lewd
  where lewd ["me"] = asks reqUser >>= act . refuse
        lewd [n]    = act (refuse n)
        refuse n    = T.concat ["refuses to enter ", n, "'s magical realm"]

commandFlip :: Command
commandFlip =
    Command
        "Flip a coin."
        ""
        (0, Just 0)
        (\_ -> liftIO ((["Heads.", "Tails."] !!) <$> randomRIO (0, 1)) >>= say)

getMenu :: IO (Map Text [Text])
getMenu = fmap (parse M.empty . T.lines) file
  where file           = T.readFile "menu.txt"
        parse m [""]   = m
        parse m ("":f) = parse m f
        parse m (n:f)  = let (sushi, rest) = break T.null f
                         in parse (M.insert n sushi m) rest

commandMenu :: Command
commandMenu = 
    Command
        "List sushi availible for order."
        ""
        (0, Just 0)
        (\_ -> liftIO getMenu >>= say . listItems . M.keys)

commandOrder :: Command
commandOrder =
    Command
        "Order some sushi."
        "[sushi]"
        (1, Just 1)
        order
  where order [sushi] = do
            menu <- liftIO getMenu
            case M.lookup (T.toLower sushi) menu of
                Just art -> sayList art
                Nothing  -> say "I'm not familiar with that kind of sushi."

commandWeebMedia :: Command
commandWeebMedia =
    Command
        "Get a random anime or manga off ANN."
        ""
        (0, Just 0)
        (const weebmedia)
  where weebmedia = do n <- liftIO (randomRIO (1, 17824) :: IO Int)
                       say (T.append root (T.pack $ show n))
        root = "http://www.animenewsnetwork.com/\
               \encyclopedia/manga.php?id="

command8ball :: Command
command8ball =
    Command
        "Ask the 8ball a question and get a wise (random) answer."
        "[question]"
        (0, Nothing)
        (\_ -> liftIO ((responses !!) <$> randomRIO (0, 19)) >>= say)
  where responses = [ "It is certain."
                    , "It is decidedly so."
                    , "Without a doubt."
                    , "Yes, definitely."
                    , "You may rely on it."
                    , "As I see it, yes."
                    , "Most likely."
                    , "Outlook good."
                    , "Yes."
                    , "Signs point to yes."
                    , "Reply hazy, try again."
                    , "Ask again later."
                    , "Better not tell you now..."
                    , "Cannot predict now."
                    , "Concentrate and ask again."
                    , "Don't count on it."
                    , "My reply is no."
                    , "My soruces say no."
                    , "Outlook not so good."
                    , "Very doubtful."
                    ]

commandCuddle :: Command
commandCuddle =
    Command
        "Send cuddles to a person, a channel, or just in general."
        "(user | channel)"
        (0, Just 1)
        cuddle
  where cuddle []  = liftIO randomHug >>= say
        cuddle [n] = do usr <- asks reqUser
                        hug <- liftIO randomHug
                        privmsg n $ T.concat ["From ", usr, ": ", hug]
        randomHug  = do n <- randomRIO (0, 1000) :: IO Int
                        if n == 0
                          then return "Fuck off"
                          else (["(>^_^)>", "<(^o^<)", "＼(^o^)／"] !!) <$> 
                               randomRIO (0, 2)

-- I'm not even touching this one...
commandGelbooru :: Command
commandGelbooru =
    Command
        "Get a random image with the specifified tags from gelbooru."
        "(tags)"
        (0, Nothing)
        (\xs -> liftIO (gelbooru xs) >>= say . T.pack)
  where gelbooru xs = do
            np   <- fromMaybe 0 <$> scrapeURL (root xs ++ "0") numPages
            let n = if np `div` 42 > 50 then 50 * 42 else np
            page <- ((root xs++) . show) <$> (randomRIO (0, n) :: IO Int)
            imgs <- scrapeURL page images :: IO (Maybe [String])
            case imgs of
                Nothing   -> noImages
                Just imgs ->
                    if null imgs
                        then noImages
                        else do img <- choice imgs
                                return $ "http://gelbooru.com/" ++ img
        root xs     = case xs of
                          [] -> "http://gelbooru.com/index.php?page=post&\
                                \s=list&pid="
                          _  -> "http://gelbooru.com/index.php?page=post&\
                                \s=list&tags=" ++
                                T.unpack (T.intercalate "+" xs) ++
                                "&pid="
        numPages    = do
            x <- attr ("href" :: String) $ ("a" :: String) @: 
                 [("alt" :: String) @= "last page"]
            let n = reverse . takeWhile isDigit . reverse $ x
            return (read n :: Int)
        images      = do
            let link = attr ("href" :: String) $ ("a" :: String) @: []
            chroots (("span" :: String) @: [hasClass ("thumb" :: String)]) link
        noImages    = return "No images with the specified tags found."

commandGetSyntax :: Command
commandGetSyntax =
    Command
        "Get the syntax of a command."
        "[command]"
        (1, Just 1)
        syntax
  where syntax [n] = case M.lookup n commands of
            Just comm ->
                say $ T.concat [ "Syntax: !", n, " "
                               , commandSyntax comm
                               ]
            Nothing -> say ("Command not found: " `T.append` n)

commandTime :: Command
commandTime =
    Command
        "Get time from the computer that SushiBot is running on."
        ""
        (0, Just 0)
        (\_ -> liftIO (readProcess "date" [] []) >>= say . T.pack)

commandTell :: Command
commandTell =
    Command
        "Queue a message for a user; it will be sent the next time they join."
        "[user] [message]"
        (2, Nothing)
        tell
  where tell (name:msg) = do
            user <- asks reqUser
            conn <- asks reqDbConn
            liftIO (queueMessage conn user name (T.unwords msg))
            say "Messaged queued."

        queueMessage conn from to msg = do
            [Only userExists] <- query conn queryExists (Only to)
            if userExists
              then execute conn queryUpdate (from, msg, to)
              else execute conn queryInsert (to, True, from, msg)

        queryExists = "SELECT (count(*) > 0) FROM users WHERE user_name = ?"
        queryUpdate = "UPDATE users SET user_has_told = 1,\
                      \user_told_from = ?, user_told_msg = ? \
                      \WHERE user_name = ?"
        queryInsert = "INSERT INTO users VALUES(?, ?, ?, ?)"

-- This blocks the main thread forever. I'm sure there's a cleaner way to
-- do this, but this is what I'm doin'.
waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

channels :: [Text]
channels = ["#lounge", "#comfy", "#secret", "#pepe", "#anime", "#bottesting"]

commands :: Map Text Command
commands = M.fromList [ ("info", commandInfo)
                      , ("source", commandSource)
                      , ("help", commandHelp)
                      , ("say", commandSay)
                      , ("act", commandAct)
                      , ("slap", commandSlap)
                      , ("mix", commandMix)
                      , ("kill", commandKill)
                      , ("fortune", commandFortune)
                      , ("lewd", commandLewd)
                      , ("flip", commandFlip)
                      , ("menu", commandMenu)
                      , ("order", commandOrder)
                      , ("weebmedia", commandWeebMedia) 
                      , ("8ball", command8ball)
                      , ("cuddle", commandCuddle)
                      , ("gelbooru", commandGelbooru)
                      , ("syntax", commandGetSyntax)
                      , ("time", commandTime)
                      , ("tell", commandTell)
                      ]

main :: IO ()
main = do conn <- open "botdata.db"
          execute_ conn "CREATE TABLE IF NOT EXISTS users (\
                        \    user_name TEXT PRIMARY KEY NOT NULL,\
                        \    user_has_told BOOL NOT NULL,\
                        \    user_told_from TEXT,\
                        \    user_told_msg TEXT\
                        \)"
          makeBot "SushiBot" channels commands [] [specialJoin]
                  "irc.sushigirl.tokyo" 6667 "botdata.db"
          waitForever

