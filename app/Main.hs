{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model

import Database.Persist.Sqlite (runSqlite, runMigration, fromSqlKey)
import Data.Text hiding (intercalate)
import Data.List (intercalate)
import System.Directory
import System.FilePath
import Options.Applicative
import Data.Time
import Database.Persist
import Control.Monad.IO.Class (liftIO)

type Description = String
data Command = StartTimer Description | StopTimer | ShowTable | ResumeTimer

data Arguments = Arguments { _command :: Command }

data Settings = Settings { _databasePath :: FilePath }


getDatabasePath :: IO FilePath
getDatabasePath = do
    home <- getAppUserDataDirectory "timetracker"
    createDirectoryIfMissing True home
    return $ home </> "database.sqlite3"


runCommand :: Settings -> Command -> IO ()
runCommand settings command = do
    currentTime <- getCurrentTime
    case command of
        StartTimer desc -> do
            runDb $ do
                runningTimeEntry <- selectFirst [TimeEntryEndedAt ==. Nothing] []
                case runningTimeEntry of
                    Just _ -> do
                        liftIO $ putStrLn "time tracker already running"
                        return ()
                    Nothing -> do
                        id <- insert $ TimeEntry (pack desc) currentTime Nothing
                        liftIO $ putStrLn $ "STARTING #" ++ (show $ fromSqlKey id) ++ ": " ++ desc
                        return ()
            return ()
        StopTimer -> do
            runDb $ do
                runningTimeEntry <- selectFirst [TimeEntryEndedAt ==. Nothing] []
                case runningTimeEntry of
                    Just (Entity id t) -> do
                        update id [TimeEntryEndedAt =. (Just currentTime)]
                        liftIO $ putStrLn $ "FINISHING #" ++ (show $ fromSqlKey id) ++ ": " ++ (unpack $ timeEntryDescription t)
                    Nothing -> do
                        liftIO $ putStrLn "error: there's no running time entry"

  where
    databasePath = pack $ _databasePath settings
    runDb = runSqlite databasePath

runApp :: Arguments -> IO ()
runApp args = do
    databasePath <- getDatabasePath
    runSqlite (pack databasePath) $ do
        runMigration migrateTables
    runCommand (Settings databasePath) (_command args)

main :: IO ()
main = do
    execParser opts >>= runApp
  where
    parser = Arguments <$> subparser (
                               command "in" (info ((StartTimer . intercalate " ") <$> descriptionParser)
                                                     (progDesc "start timer"))
                            <> command "out" (info (pure StopTimer)
                                                    (progDesc "stop timer"))
                           )
    descriptionParser = some (argument str (metavar "DESC"))
    opts = info parser mempty
