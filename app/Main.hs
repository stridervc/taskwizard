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
  putStrLn "  (nothing)   - List tasks"
  putStrLn "  add [desc]  - Add task"
  putStrLn "  [id] delete - Delete task with ID"
  putStrLn "  [id] done   - Mark task with ID as done"
  putStrLn "  refactor    - Renumber tasks and clean up the save file"
  putStrLn "  [id] start  - Mark a task as started"
  putStrLn "  [id] stop   - Mark a task as not started"
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
