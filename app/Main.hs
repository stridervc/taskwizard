module Main where

import Task

main :: IO ()
main = do
  putStrLn $ show $ taskFromString 1 "Test task"
