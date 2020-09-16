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
       "-> help >> show this message", "-> succ {n} >> return n+1",
       "-> magic man >> print the bible of Magic Man",
       "-> multi {emoji} {n} >> print out n emoji",
       "-> rect {emoji} {x} {y} >> print an x by y rectanlge of emoji"]

magicText :: T.Text
magicText
  = "In the beginning, there was the Ceiling. And Magic Man said, \"Magic\". And then there was magic. And Magic Man said, \"Magic Town\". And then there was Magic Town. And Magic Man wandered Magic Town, and there wasn't enough magic. So Magic Man said, \"Let there be Magic People\". And there was magic people. And the Magic People understood that they were magic, and that Magic Man was magic, and that Magic Town was magic. And then Magic Man realized that there weren't enough ceilings. So magic man thought, \"what if the entire town was a ceiling\"? And so Magic Man said, \"Let there be Ceilings\". And then Magic Town was the ceiling to the entire world in which we now live."

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

emojiGen :: String -> Maybe T.Text
emojiGen x
  = fmap T.pack $
      fmap (\ y -> "<:" ++ x ++ ":" ++ y ++ ">") $ lookup x emojiDict

restrict :: Ord a => a -> a -> a -> a
restrict l u x
  = if | (x >= l) && (x <= u) -> x
       | (x >= l) -> u
       | (x <= u) -> l
       | otherwise -> undefined

main :: IO ()
main = test

test :: IO ()
test
  = do userFacingError <- runDiscord $
                            def{discordToken =
                                  "",
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
                         "magic" -> case head args of
                                        "man" -> Just magicText
                                        otherwise -> Nothing
                         otherwise -> Nothing
                3 -> case command of
                         "multi" -> if | isJust emoji ->
                                         Just $
                                           T.concat $
                                             replicate emojiCount $
                                               fromJust emoji
                                       | otherwise -> Nothing
                           where emojiCount
                                   = restrict 1 100 $
                                       fromMaybe 1 $ S.readMay $ last args
                                       :: Int
                                 emoji = emojiGen $ head args
                         otherwise -> Nothing
                4 -> case command of
                         "rect" -> if | isJust emoji ->
                                        Just $
                                          T.unlines $
                                            map T.concat $
                                              replicate y $
                                                replicate x $ fromJust emoji
                                      | otherwise -> Nothing
                           where [x, y]
                                   = map
                                       (\ a ->
                                          restrict 1 25 $
                                            fromMaybe 1 $ S.readMay $ args !! a
                                            :: Int)
                                       [1, 2]
                                 emoji = emojiGen $ head args
                         otherwise -> Nothing
                otherwise -> Nothing

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: T.Text -> Bool
isCommand = T.isPrefixOf "->"
