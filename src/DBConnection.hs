module DBConnection where

import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.IO.Class
import           Data.Text
import           Data.Time
import           Database.Persist.Sqlite
import           Database.Persist.Sql
import           Database.Persist.TH
import           Schema

-- import           Horoscope ( signByInt
--                            , intBySign
--                            )


dbFilePath :: Text
dbFilePath = pack "data/magicbot.db"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

getHoroscopeBySignCodAndTime :: Int -> UTCTime -> IO (Maybe DailyHoroscope)
getHoroscopeBySignCodAndTime signCod tm = do 
  runStdoutLoggingT $ filterLogger logFilter $ withSqliteConn dbFilePath  $ runReaderT actions
  where
    actions :: SqlPersistT (LoggingT IO) (Maybe DailyHoroscope)
    actions = do
      let tmid = UTCTime (utctDay tm) 0
      prognosises <- selectList [DailyHoroscopeSignCode ==. signCod , DailyHoroscopePredictionTime ==. tmid] []
      case prognosises of
        hor:_ -> return $ Just $ entityVal hor
        _     -> return Nothing 


insertHoroscope :: Int -> Text -> UTCTime -> IO (Key DailyHoroscope)
insertHoroscope signId hText timesTamp = do
  let curTimeMid = UTCTime (utctDay timesTamp) 0
  runStdoutLoggingT $ filterLogger logFilter $ withSqliteConn dbFilePath  $ runReaderT (actions signId hText curTimeMid) 
  where
    actions :: Int -> Text -> UTCTime -> SqlPersistT (LoggingT IO) (Key DailyHoroscope)
    actions signId' hText' timesTamp' = do
      prognosises <- selectList [DailyHoroscopeSignCode ==. signId' , DailyHoroscopePredictionTime ==. timesTamp'] []
      case prognosises of
        hor:_ ->  return $ entityKey hor
        _     ->  let daylyHoroscope = DailyHoroscope signId' hText' timesTamp' 
                    in insert  daylyHoroscope