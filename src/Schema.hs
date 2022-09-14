{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Schema where

import           Control.Monad.IO.Class
import           Data.Text
import           Data.Time
import           Database.Persist.Sqlite
import           Database.Persist.Sql
import           Database.Persist.TH


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    QuestionAndAnswer
      userId Int
      username Text Maybe
      questionId Int
      questionText Text
      answerText Text
      sent UTCTime 

      deriving Show


    DailyHoroscope
      signCode Int
      predictionH Text 
      predictionTime UTCTime 

      deriving Show
        
    |]

--  DailyHoroscope {dailyHoroscopeSignCode :: !Int,
--                     dailyHoroscopePredictionH :: !Text,
--                     dailyHoroscopePredictionTime :: !time-1.9.3:Data.Time.Clock.Internal.UTCTime.UTCTime}


doMigration :: MonadIO m => SqlPersistT m ()
doMigration = runMigration migrateAll
