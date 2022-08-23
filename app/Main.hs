{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Maybe

import           Telegram.Bot.API 
import           Telegram.Bot.Simple 
import           Telegram.Bot.Simple.UpdateParser ( updateMessageText
                                                  , updateMessageSticker
                                                  , command
                                                  , text
                                                  , callbackQueryDataRead
                                                  , parseUpdate
                                                  , mkParser)
import           Telegram.Bot.API.InlineMode.InlineQueryResult 
import           Telegram.Bot.API.Types
import           Telegram.Bot.Simple.Reply
import           Telegram.Bot.Simple.InlineKeyboard
import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import           Network.HTTP.Client ( responseTimeoutNone
                                     , newManager
                                     , managerResponseTimeout)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client ( ClientEnv
                                , mkClientEnv)
import           System.Random (randomRIO)
import           Control.Monad.IO.Class
import           Control.Monad

import           Horoscope ( randomResponse
                           , randomHoroscope
                           , signs
                           , secureTokenName)

type Model = ()

data Action
  = NoAction
  | StickerEcho ChatId
  | AudioEcho Voice
  | Answer Text
  | Start ChatId
  | Horoscope 
  | HoroscopeAction Text
  deriving(Show)


magicBot :: BotApp Model Action
magicBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction 
  , botHandler = handleAction
  , botJobs = 
      [ BotJob
        { botJobSchedule = "Stars say:"
        , botJobTask = todoHoroscope
        }
      ]
  }

updateMessageVoice :: Update -> Maybe Voice 
updateMessageVoice update = updateMessage update >>= messageVoice

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateMessageSticker update = do
    fileId <- stickerFileId <$> updateMessageSticker update
    chatId <- updateChatId update
    pure $ StickerEcho chatId
  | isJust $ updateMessageVoice update = do
    voiceMsg <- updateMessageVoice update
    pure $ AudioEcho voiceMsg
  | isJust maybeCallback = case maybeCallback of
      Just msg -> Just $ HoroscopeAction $ Text.pack [(Text.head msg)]
      Nothing  -> Nothing
  | otherwise = case updateMessageText update of
      Just "/start" -> Start <$> updateChatId update
      Just "/horoscope" -> Just Horoscope
      Just text -> Just (Answer text)
      Nothing   -> Nothing
  where maybeCallback = parseUpdate myParser update
        myParser = mkParser $ \ upd -> do
            query <- updateCallbackQuery upd
            data_ <- callbackQueryData query
            return data_
        
startMessage :: Text
startMessage = Text.unlines
 [ "I am magic sphere 🔮 ."
 , "Ask me any questions!"
 ]

gachiBillyFinger :: InputFile
gachiBillyFinger = InputFileId  $ FileId "CAACAgIAAxkBAAIBWGLf7LPOO-GrFRX8SRbUmwsOZ_CHAAIZFgACerrIS3hkuqvwkenQKQQ"

gachiBillyBear :: InputFile
gachiBillyBear = InputFileId  $ FileId "CAACAgIAAxkBAAIBWWLf7LMl4zzQ8DX9VOwL7Q4zaEXBAALMFAACCtfJSxrsaCgezU3_KQQ"

-- | Actions to do with an item as an inline keyboard.
horoscopeActionsKeyboard :: InlineKeyboardMarkup
horoscopeActionsKeyboard = InlineKeyboardMarkup buttns
    where
      buttns = [ map (\sgn -> (labeledInlineKeyboardButton sgn){inlineKeyboardButtonCallbackData = Just (Text.append sgn  "HoroscopeAction")} ) s | 
                 s <- signs 
               ]

todoHoroscope :: Model -> Eff Action Model
todoHoroscope model = model <# do
    hscp <- randomHoroscope
    replyText hscp
    pure NoAction
-- todo  
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  StickerEcho chat -> model <# do
    _ <- liftClientM 
      (sendSticker 
        (SendStickerRequest 
          (SomeChatId chat) 
          gachiBillyFinger 
          Nothing
          Nothing
          Nothing 
          Nothing 
          Nothing))
    pure NoAction
  AudioEcho voiceMsg  ->  model <# do
    replyText "received voice message"
    pure NoAction
  Answer msg -> model <# do
    resp <- randomResponse
    replyText $ resp
    pure NoAction
  Start chat -> model <# do
    replyText startMessage
    _ <- liftClientM 
        (sendSticker 
            (SendStickerRequest 
            (SomeChatId chat) 
            gachiBillyBear
            Nothing
            Nothing
            Nothing 
            Nothing 
            Nothing))
    pure NoAction
  Horoscope -> model <# do
    reply (toReplyMessage "Choose your Astrological sign")
       { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup horoscopeActionsKeyboard
       }
    pure NoAction
  HoroscopeAction sgn -> model <# do
    hscp <- randomHoroscope
    replyText $ Text.append sgn $ Text.append  ": "  hscp
    pure NoAction

myEnv :: Token -> IO ClientEnv
myEnv token = mkClientEnv
  <$> newManager (tlsManagerSettings{managerResponseTimeout=responseTimeoutNone})
  <*> pure (botBaseUrl token)


run :: Token -> IO ()
run token = do
  env <- myEnv token
  --let myEnv = env {mResponseTimeout = ResponseTimeoutNone}
  res <- startBot magicBot env
  print res

main :: IO ()
main = do
  tokenVal <- secureTokenName
  case tokenVal of  
    Just mytoken -> do 
      putStrLn $ "Telegram bot's API token: " ++ mytoken
      run $ Token . Text.pack $ mytoken
    Nothing -> putStrLn $ "Add your token to the file \"botToken\" " 
