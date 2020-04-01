module Main where

import Task

main :: IO ()
main = do
  putStrLn $ show $ addTask [] "Test task"
