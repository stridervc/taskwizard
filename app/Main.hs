{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Task
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
  putStrLn "  delete [id] - Delete task with ID"
  putStrLn "  done [id]   - Mark task with ID as done"
  putStrLn "  refactor    - Renumber tasks and clean up the save file"
  putStrLn "  start       - Mark a task as started"
  putStrLn "  stop        - Mark a task as not started"
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
      let eitherAction = parseExactAction now tasks $ unwords args
      let cmd  = head args
      let rest = unwords $ tail args
      --let action = parseAction $ unwords args
      
      case cmd of
        "refactor" -> do
          saveActions filename $ tasksToActions $ refactor now tasks
        "help" -> showHelp

        otherwise  -> do
          case eitherAction of
            Right a -> saveActions filename $ actions ++ [a]
            Left e  -> putStrLn e
