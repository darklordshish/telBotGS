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


main :: IO ()
main = do
  runSqlite dbFilePath $ do
    doMigration
  mainBot
