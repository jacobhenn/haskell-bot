--------------------------------------------------------------------------------
-- Personal Discord Bot written in Haskell
-- Author: Jacob Henn
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- some functions use Either error handling, which I don't need
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a) = Nothing
eitherToMaybe (Right b) = Just b

--------------------------------------------------------------------------------
-- the text printed by the help function
helpText :: T.Text
helpText
  = T.unlines
      ["Hello, I'm a bot written entirely in haskell!",
       "Here is a list of my commands (`->` is my prefix)!",
       "-> help >> show this message",
       "-> magic man >> print the bible of Magic Man",
       "-> multi {emoji} {n} >> print out n emoji",
       "-> rect {emoji} {x} {y} >> print an x by y rectanlge of emoji"]

--------------------------------------------------------------------------------
-- the bible of Magic Man
magicText :: T.Text
magicText
  = "In the beginning, there was the Ceiling. And Magic Man said, \"Magic\". And then there was magic. And Magic Man said, \"Magic Town\". And then there was Magic Town. And Magic Man wandered Magic Town, and there wasn't enough magic. So Magic Man said, \"Let there be Magic People\". And there was magic people. And the Magic People understood that they were magic, and that Magic Man was magic, and that Magic Town was magic. And then Magic Man realized that there weren't enough ceilings. So magic man thought, \"what if the entire town was a ceiling\"? And so Magic Man said, \"Let there be Ceilings\". And then Magic Town was the ceiling to the entire world in which we now live."

--------------------------------------------------------------------------------
-- a dictionary that matches the name of server emoji with their emoji ID (get 
-- emoji ID by typing `\:emoji:`)
emojiDict :: [(String, String)]
emojiDict
  = [("wilson", "740374394592821319"), ("chair_alex", "755864191587582074"),
     ("kongus_diagonalus", "740368871231324230"),
     ("coolkid", "739657840800825365"), ("stonalisa", "739689758565072916"),
     ("stevehead", "739653619896418324"), ("steveteeth", "739653810745638972"),
     ("steveeyes", "739653743150235650"), ("stevechin", "739653854987288656"),
     ("spongehead", "739654027993808926"), ("spongelegs", "739654233661636729"),
     ("phant", "739642554177552526"), ("linusquits", "739653472894451742"),
     ("lemonke", "739642776613945346"), ("jesus", "739653385464053934"),
     ("heaventime", "739642665783525439"),
     ("hangarinnose", "739690392210899065"), ("carlo", "739654565149802516"),
     ("badam", "739918421445312573")]

--------------------------------------------------------------------------------
-- take an emoji name and format it to be displayed correctly in Discord
emojiGen :: String -> Maybe T.Text
emojiGen x
  = fmap T.pack $
      fmap (\ y -> "<:" ++ x ++ ":" ++ y ++ ">") $ lookup x emojiDict

--------------------------------------------------------------------------------
-- check if a message lies within the internal 2k char limit
limitCheck :: T.Text -> T.Text
limitCheck x
  | T.length x < 2000 = x
  | otherwise = "Length of output must not exceed 2000 characters."

--------------------------------------------------------------------------------
-- main function
main :: IO ()
main = secureMain

--------------------------------------------------------------------------------
-- secure main function
secureMain :: IO ()
secureMain
  = do userFacingError <- runDiscord $
                            def{discordToken =
                                  "",
                                discordOnEvent = eventHandler}
       TIO.putStrLn userFacingError

--------------------------------------------------------------------------------
-- hook that is called on an Event, currently only a new message event.
eventHandler :: Event -> DiscordHandler ()
eventHandler event
  = case event of
        MessageCreate m -> when (not (isFromBot m) && isCommand (messageText m))
                             $
                             do _ <- restCall
                                       (R.CreateMessage (messageChannel m)
                                          (respondToMessage m))
                                pure ()
        _ -> pure ()

--------------------------------------------------------------------------------
-- function that is called on the message text and returns the reply
respondToMessage :: Message -> T.Text
respondToMessage message
  = fromMaybe "Something went wrong." $ fmap limitCheck go
  where messageString = T.unpack (messageText message)
        (command : args) = drop 1 $ words messageString
        go
          = case length (words messageString) - 1 of
                1 -> case command of
                         "help" -> Just helpText
                         otherwise -> Nothing
                2 -> case command of
                         --------------------------------------------------------------------------------
                         -- print the bible of Magic Man
                         "magic" -> case head args of
                                        "man" -> Just magicText
                                        otherwise -> Nothing
                         otherwise -> Nothing
                3 -> case command of
                         --------------------------------------------------------------------------------
                         -- print out a specified emoji a specified number of
                         -- times
                         "multi" -> if | isJust emoji ->
                                         Just $
                                           T.concat $
                                             replicate emojiCount $
                                               fromJust emoji
                                       | otherwise -> Nothing
                           where emojiCount
                                   = fromMaybe 1 $ S.readMay $ last args :: Int
                                 emoji = emojiGen $ head args
                         otherwise -> Nothing
                4 -> case command of
                         --------------------------------------------------------------------------------
                         -- print out a rectangle of specified dimensions of a
                         -- specified emoji
                         "rect" -> if | isJust emoji && and (map isJust [x, y])
                                        ->
                                        Just $
                                          T.unlines $
                                            map T.concat $
                                              replicate (fromJust y) $
                                                replicate (fromJust x) $
                                                  fromJust emoji
                                      | otherwise -> Nothing
                           where [x, y]
                                   = map S.readMay $ take 2 $ drop 1 args ::
                                       [Maybe Int]
                                 emoji = emojiGen $ head args
                         otherwise -> Nothing
                otherwise -> Nothing

--------------------------------------------------------------------------------
-- is the message from a bot user?
isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

--------------------------------------------------------------------------------
-- does the message contain the relevant prefix?
isCommand :: T.Text -> Bool
isCommand = T.isPrefixOf "->"
