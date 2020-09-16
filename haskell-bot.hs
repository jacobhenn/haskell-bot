{-# LANGUAGE OverloadedStrings #-}
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
       "-> help >> show this message", "-> succ {n} >> return n+1", "-> magic man >> print the bible of Magic Man"]

magicText :: T.Text
magicText = "In the beginning, there was the Ceiling. And Magic Man said, \"Magic\". And then there was magic. And Magic Man said, \"Magic Town\". And then there was Magic Town. And Magic Man wandered Magic Town, and there wasn't enough magic. So Magic Man said, \"Let there be Magic People\". And there was magic people. And the Magic People understood that they were magic, and that Magic Man was magic, and that Magic Town was magic. And then Magic Man realized that there weren't enough ceilings. So magic man thought, \"what if the entire town was a ceiling\"? And so Magic Man said, \"Let there be Ceilings\". And then Magic Town was the ceiling to the entire world in which we now live."

main :: IO ()
main = test

test :: IO ()
test
  = do userFacingError <- runDiscord $
                            def{discordToken =
                                discordOnEvent = eventHandler}
       TIO.putStrLn userFacingError
eventHandler :: Event -> DiscordHandler ()
eventHandler event
  = case event of
        MessageCreate m -> when (not (fromBot m) && isCommand (messageText m)) $
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
                1 -> case command of
                         "help" -> Just helpText
                         otherwise -> Nothing
                2 -> case command of
                         "succ" -> fmap T.pack $
                                     fmap (show . succ)
                                       (S.readMay $ head args :: Maybe Integer)
                         "magic" -> case head args of "man" -> Just magicText
                                                      otherwise -> Nothing
                         otherwise -> Nothing
                otherwise -> Nothing

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: T.Text -> Bool
isCommand = T.isPrefixOf "->"
