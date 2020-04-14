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

main :: IO ()
main = do
  args <- getArgs
  now <- getCurrentTime

  filename <- taskFilename
  actions <- loadActions filename
  let tasks = tasksFromActions now actions

  case length args of
    0 -> do
      putStrLn ""
      printTasks now tasks
      putStrLn ""

    otherwise -> do
      let (cmd,filter,arguments) = eval $ unwords args
      case cmd of
        "list" -> do
          putStrLn ""
          printTasks now tasks
          putStrLn ""

        "refactor" -> do
          saveActions filename $ tasksToActions $ refactor now tasks
        "help" -> showHelp

        --saveActions filename $ actions ++ [a]
