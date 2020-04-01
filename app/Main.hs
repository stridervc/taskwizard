module Main where

import Task

main :: IO ()
main = do
  printTasks $ addTask [] "Test task"
