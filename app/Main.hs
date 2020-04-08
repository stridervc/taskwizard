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

main :: IO ()
main = do
  args <- getArgs
  now <- getCurrentTime

  filename <- taskFilename
  actions <- loadActions filename
  let tasks = tasksFromActions now actions

  case length args of
    0 -> printTasks tasks
    otherwise -> do
      let eitherAction = parseExactAction now tasks $ unwords args
      --let cmd  = head args
      --let rest = unwords $ tail args
      --let action = parseAction $ unwords args
      
      case eitherAction of
        Right a -> saveActions filename $ actions ++ [a]
        Left e  -> putStrLn e
