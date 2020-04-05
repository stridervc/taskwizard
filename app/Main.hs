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

main :: IO ()
main = do
  args <- getArgs

  filename <- taskFilename
  actions <- loadActions filename
  let tasks = tasksFromActions actions

  case length args of
    0 -> printTasks tasks
    otherwise -> do
      let cmd  = head args
      let rest = unwords $ tail args
      let action = parseAction $ unwords args

      case cmd of
        "add" -> do
          saveActions filename $ actions ++ [action]

        "delete" -> do
          let id = read $ args!!1
          case uidExists tasks id of
            False -> putStrLn "Task ID doesn't exist"
            True  -> do
              saveActions filename $ actions ++ [action]

        "done" -> do
          let id = read $ args!!1
          case uidExists tasks id of
            False -> putStrLn "Task ID doesn't exist"
            True  -> do
              saveActions filename $ actions ++ [action]

        otherwise -> do
          putStrLn $ "Unknown action: " ++ cmd
