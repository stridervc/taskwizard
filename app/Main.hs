{-# LANGUAGE OverloadedStrings #-}

module Main where

import Task
import System.Directory (getHomeDirectory, doesFileExist)
import Data.Binary

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
  filename <- taskFilename
  loadres <- loadActions filename
  case loadres of
    Left (offset, error) -> putStrLn error
    Right actions -> do
      putStrLn $ show (actions :: [Action])
      -- TODO apply action from command line
      -- save actions
