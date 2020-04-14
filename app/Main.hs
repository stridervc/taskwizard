{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Task
import Parser
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment
import System.IO.Strict (readFile)
import Data.Time

taskFilename :: IO String
taskFilename = do
  home <- getHomeDirectory
  return $ home ++ "/.taskwizard"

saveActions :: String -> Actions -> IO ()
saveActions f as = writeFile f $ unlines $ dumpActions as

loadActions :: String -> IO Actions
loadActions f = do
  exists <- doesFileExist f
  case exists of
    False -> return []
    True  -> do
      s <- readFile f
      return $ unDumpActions $ lines s

showHelp :: IO ()
showHelp = do
  putStrLn "taskwizard help"
  putStrLn "  (nothing)       - List tasks"
  putStrLn "  add desc        - Add task"
  putStrLn "  <filter> delete - Delete task with ID"
  putStrLn "  <filter> done   - Mark tasks as done"
  putStrLn "  refactor        - Renumber tasks and clean up the save file"
  putStrLn "  <filter> start  - Mark tasks as started"
  putStrLn "  <filter> stop   - Mark tasks as not started"
  putStrLn ""
  putStrLn "where filter is a comma separated list of ids or keywords"
  putStrLn ""

filterTasks :: [Filter] -> Tasks -> Tasks
filterTasks _ [] = []
filterTasks [] _ = []
filterTasks (f:fs) ts = do
  case f of
    Fid i -> taskFromID ts i : filterTasks fs ts
    Ftext s -> (filter (descContains s) ts) ++ filterTasks fs ts

printTasks' now ts = do
  putStrLn ""
  printTasks now ts
  putStrLn ""

-- apply action to ids of filtered tasks and return actions
taskActions :: (ID -> Action) -> [Filter] -> Tasks -> Actions
taskActions a fs ts = map a ids
  where ts' = filterTasks fs ts
        ids = map uid ts'

doneTask = Done
doneTasks = taskActions doneTask

startTask = Start
startTasks = taskActions startTask

stopTask = Stop
stopTasks = taskActions stopTask

deleteTask = Delete
deleteTasks = taskActions deleteTask

main :: IO ()
main = do
  args <- getArgs
  now <- getCurrentTime

  filename <- taskFilename
  actions <- loadActions filename
  let tasks = tasksFromActions now actions

  case length args of
    0 -> printTasks' now tasks

    otherwise -> do
      case eval $ unwords args of
        ("list", [], "") ->
          printTasks' now tasks

        ("list", fs, "") ->
          printTasks' now $ filterTasks fs tasks

        ("refactor", [], "") ->
          saveActions filename $ tasksToActions $
            refactor now tasks

        ("help", [], "") ->
          showHelp

        ("add", [], a) -> do
          let action = parseAddAction now tasks a
          saveActions filename $ actions ++ [action]

        ("done", fs, "") ->
          saveActions filename $ actions ++ doneTasks fs tasks

        ("start", fs, "") ->
          saveActions filename $ actions ++ startTasks fs tasks

        ("stop", fs, "") ->
          saveActions filename $ actions ++ stopTasks fs tasks

        ("delete", fs, "") ->
          saveActions filename $ actions ++ deleteTasks fs tasks

