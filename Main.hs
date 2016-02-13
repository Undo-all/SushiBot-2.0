{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot
import Control.Monad
import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Text as T

-- This blocks the main thread forever. I'm sure there's a cleaner way to
-- do this, but this is what I'm doin'.
waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

main :: IO ()
main = do makeBot "SushiBot-2" ["#lounge", "#comfy"] (M.fromList [("say", Command "" (1, Nothing) (say . T.unwords))]) [] "irc.sushigirl.tokyo" 6667
          waitForever

