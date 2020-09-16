{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
import Control.Monad (when)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Safe as S

import UnliftIO (liftIO)
import UnliftIO.Concurrent

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a) = Nothing
eitherToMaybe (Right b) = Just b

helpText :: T.Text
helpText
  = T.unlines
      ["Hello, I'm a bot written entirely in haskell!",
       "Here is a list of my commands (`->` is my prefix)!",
       "-> help >> show this message", "-> succ {n} >> return n+1"]

main :: IO ()
main = test

test :: IO ()
test
  = do userFacingError <- runDiscord $
                            def{discordToken =
                                  "Njc4MzMwNTkzNDEwNDE2NjY5.XkhOmA.X4lH_i1kpysnyXLE4Tw0aifl1qI",
                                discordOnEvent = eventHandler}
       TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event
  = case event of
        MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $
                             do _ <- restCall
                                       (R.CreateMessage (messageChannel m)
                                          (respondToMessage m))
                                pure ()
        _ -> pure ()

respondToMessage :: Message -> T.Text
respondToMessage message = fromMaybe "Something went wrong." go
  where messageString = T.unpack (messageText message)
        (command : args) = drop 1 $ words messageString
        go
          = case length (words messageString) - 1 of
                0 -> Nothing
                1 -> case command of
                         "help" -> Just helpText
                         otherwise -> Nothing
                2 -> case command of
                         "succ" -> fmap T.pack $
                                     fmap (show . succ)
                                       (S.readMay $ head args :: Maybe Integer)
                         otherwise -> Nothing
                otherwise -> Nothing

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "->"
