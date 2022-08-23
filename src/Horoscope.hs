{-# LANGUAGE OverloadedStrings #-}

module Horoscope
    ( randomResponse
    , randomHoroscope 
    , signs
    , secureTokenName 
    ) where


import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           System.Random (randomRIO)
import           Control.Monad.IO.Class (MonadIO(..))
import           System.Directory (doesFileExist)

responses :: [Text]
responses = [
    "It is certain",
    "Without a doubt",
    "You may rely on it",
    "Yes definitely",
    "It is decidedly so",
    "As I see it, yes",
    "Most likely",
    "Yes",
    "Outlook good",
    "Signs point to yes",
    "Reply hazy try again",
    "Better not tell you now",
    "Ask again later",
    "Cannot predict now",
    "Concentrate and ask again",
    "Don't count on it",
    "Outlook not so good",
    "My sources say no",
    "Very doubtful",
    "My reply is no"
    ]

first :: [Text]
first = [ "Today is perfect for new endeavors. "
        , "The tensions of this week will feel heavier today than yesterday. "
        , "Today is the day to cherish and embrace others. "
        , "Making yourself useful is a main component of a successful day. "
        , "Today, exercise caution when crossing the street. "]

second :: [Text]
second = [ "Remember that good things come to those who work hard. "
         , "Don’t let the circumstances bring you down. "
         , "Patience is key, but sometimes a little push can get the job done. "
         , "A smile can get you a long way. "]

third :: [Text]   
third = [ "Looking ahead may seem like a waste of time, but it pays off in the end. "
        , "Luck favors those who mind the risks and take them. "
        , "Today is the day for that thing you always wanted to do. "
        , "Luck is on your side today, so seize it! "
        , "Things are looking up for you! "]


randomResponse :: MonadIO m => m Text
randomResponse = do
    randInt <- liftIO $ randomRIO (0,19)
    return $ responses !! randInt

randomHoroscope :: MonadIO m => m Text
randomHoroscope = do
  i <- liftIO $ randomRIO (1,length first)
  j <- liftIO $ randomRIO (1,length second)
  k <- liftIO $ randomRIO (1,length third)
  return $ Text.unlines [first !! (i-1), second !! (j-1), third !! (k-1)]

signs :: [[Text]]
signs = [["♈️","♉️","♊️","♋️"]
        ,["♌️","♍️","♎️","♏️"]
        ,["♐️","♑️","♒️","♓️"]
        ]

signs' :: [(Int,Text)]
signs' = [( 3,"♈️"),( 4,"♉️"),( 5,"♊️"),( 6,"♋️")
         ,( 7,"♌️"),( 8,"♍️"),( 9,"♎️"),(10,"♏️")
         ,(11,"♐️"),(12,"♑️"),( 1,"♒️"),( 2,"♓️")
         ]

secureTokenName :: IO (Maybe String)
secureTokenName  = do
    fileExist <- doesFileExist "botToken" 
    if fileExist then do
        myToken <- readFile "botToken"
        return (Just myToken)
    else 
        return Nothing