{-# LANGUAGE OverloadedStrings #-}

module Main where

import Task
import System.Directory (getHomeDirectory, doesFileExist)
import Data.Binary
import System.Environment

taskFilename :: IO String
taskFilename = do
  home <- getHomeDirectory
  return $ home ++ "/.taskwizard"

-- loadActions :: String -> IO (Either (offset, String) [Action])
loadActions f = do
  exists <- doesFileExist f
  if exists then
    decodeFileOrFail f
  else
    return $ Right []

main :: IO ()
main = do
  args <- getArgs

  -- TODO fix bug with duplicate UUID when adding task
  -- it's in applyActions
  
  filename <- taskFilename
  loadres <- loadActions filename
  case loadres of
    Left (offset, error) -> putStrLn error
    Right actions -> do
      let tasks = applyActions [] actions
      case tasks of
        Left e -> putStrLn e
        Right tasks -> do
          case length args of
            0 -> printTasks tasks
            otherwise -> do
              case args!!0 of
                "add" -> do
                  let as = actions ++ [Add $ concat $ tail args]
                  encodeFile filename as
