{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Task
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment
import System.IO.Strict (readFile)

taskFilename :: IO String
taskFilename = do
  home <- getHomeDirectory
  return $ home ++ "/.taskwizard"

saveTasks :: String -> Tasks -> IO ()
saveTasks f ts = writeFile f $ unlines $ dumpTasks ts

loadTasks :: String -> IO Tasks
loadTasks f = do
  exists <- doesFileExist f
  case exists of
    False -> return []
    True -> do
      s <- readFile f
      return $ unDumpTasks $ lines s

main :: IO ()
main = do
  args <- getArgs

  filename <- taskFilename
  tasks <- loadTasks filename

  case length args of
    0 -> printTasks tasks
    otherwise -> do
      case args!!0 of
        "add" -> do
          let s = unwords $ tail args
          saveTasks filename $ parseAddTask tasks s

        "delete" -> do
          let id = read $ args!!1
          case uidExists tasks id of
            False -> putStrLn "Task ID doesn't exist"
            True  -> do
              saveTasks filename $ deleteTask tasks id

        "done" -> do
          let id = read $ args!!1
          case uidExists tasks id of
            False -> putStrLn "Task ID doesn't exist"
            True  -> do
              saveTasks filename $ doTask tasks id

        otherwise -> do
          putStrLn $ "Unknown action: " ++ args!!0
