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
                  let a = stringToExact tasks $ unwords $ tail args
                  let as = actions ++ [a]
                  encodeFile filename as

                otherwise -> do
                  putStrLn $ "Unknown action: " ++ args!!0
