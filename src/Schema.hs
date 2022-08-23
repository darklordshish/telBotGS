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
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
      User
        userId Int
        username Text Maybe
        created UTCTime default=now()

        Primary userId
        deriving Show

      QuestionAndAnswer
        questionId Int
        userId UserId
        questionText Text
        answerText Text
        sent UTCTime default=now()

        UniqueMsgUser questionId userId

      DailyHoroscope
        horoscopeId Int
        signCode Int
        predictionH Text 
        sent UTCTime default=now()

        Primary horoscopeId
        deriving Show
        
    |]

doMigration :: MonadIO m => SqlPersistT m ()
doMigration = runMigration migrateAll