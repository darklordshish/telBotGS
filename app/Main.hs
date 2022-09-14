{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad.Logger
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Maybe

import           Database.Persist.Sqlite

import           Bot                         (mainBot)
import           Schema                      (doMigration)
import           DBConnection                (dbFilePath)

--connStr = "host=127.0.0.1 port=5432 user=semion dbname=magicbot password=semion " 
-- | latency(min)(cost -> min par) . par(?) . rps(max)(10-15%) . (length queue -> par,rps) . rps(sucsess )

main :: IO ()
main = do
  runSqlite dbFilePath $ do
    doMigration
  mainBot
