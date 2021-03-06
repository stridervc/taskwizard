{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Task
import Parser
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment
import System.IO.Strict (readFile)
import Data.Time
import Data.List.Extra (lower, isInfixOf)

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
  putStrLn "  (nothing)            - List tasks"
  putStrLn "  add [desc]           - Add task"
  putStrLn "  projects             - Show list of projects and number of tasks in them"
  putStrLn "  refactor             - Renumber tasks and clean up the save file"
  putStrLn "  <filter> delete      - Delete task with ID"
  putStrLn "  <filter> done        - Mark tasks as done"
  putStrLn "  <filter> start       - Mark tasks as started"
  putStrLn "  <filter> stop        - Mark tasks as not started"
  putStrLn "  <filter> modify desc - Modify task"
  putStrLn "  <filter> show        - Show task details"
  putStrLn ""
  putStrLn "where filter is a comma separated list of ids or keywords"
  putStrLn ""

filterTasks :: [Filter] -> Tasks -> Tasks
filterTasks _ [] = []
filterTasks [] _ = []
filterTasks (f:fs) ts = do
  case f of
    Fid i       -> taskFromID ts i : fs'
    Ftext s     -> filter (descmatch $ lower s) ts ++ fs'
    Fproject p  -> filter (projmatch $ lower p) ts ++ fs'
  where lowerdesc = (\t -> lower $ show t)
        descmatch = (\s t -> s `isInfixOf` lowerdesc t)
        projmatch = (\s t -> s `isInfixOf` (lower $ project t))
        fs'       = filterTasks fs ts

printTasks' now ts = do
  putStrLn ""
  printTasks now ts
  putStrLn ""

-- apply action to ids of filtered tasks and return actions
taskActions :: (ID -> Action) -> [Filter] -> Tasks -> Actions
taskActions a fs ts = map a ids
  where ts' = filterTasks fs ts
        ids = map uid ts'

doneTask = Done
doneTasks = taskActions doneTask

startTask = Start
startTasks = taskActions startTask

stopTask = Stop
stopTasks = taskActions stopTask

deleteTask = Delete
deleteTasks = taskActions deleteTask

modifyTasks fs s ts = taskActions a fs ts
  where a = (\i -> Modify i s)

main :: IO ()
main = do
  args <- getArgs
  now <- getCurrentTime

  filename <- taskFilename
  actions <- loadActions filename
  let tasks = tasksFromActions now actions

  case length args of
    0 -> printTasks' now tasks
    _ -> do
      case eval $ unwords args of
        Right ("list", [], "") ->
          printTasks' now tasks

        Right ("list", fs, "") ->
          printTasks' now $ filterTasks fs tasks

        Right ("", fs, "") ->
          printTasks' now $ filterTasks fs tasks

        Right ("refactor", [], "") ->
          saveActions filename $ tasksToActions $
            refactor now tasks

        Right ("help", [], "") ->
          showHelp

        Right ("add", [], a) -> do
          let action = parseAddAction now tasks a
          saveActions filename $ actions ++ [action]
          let nts = tasksFromActions now $ actions ++ [action]
          let nt = head nts
          putStrLn $ "Added task with ID " ++ show (uid nt)

        Right ("done", fs, "") ->
          saveActions filename $ actions ++ doneTasks fs tasks

        Right ("start", fs, "") ->
          saveActions filename $ actions ++ startTasks fs tasks

        Right ("stop", fs, "") ->
          saveActions filename $ actions ++ stopTasks fs tasks

        Right ("delete", fs, "") ->
          saveActions filename $ actions ++ deleteTasks fs tasks

        Right ("modify", fs, s) ->
          saveActions filename $ actions ++ modifyTasks fs s tasks

        Right ("show", fs, "") ->
          mapM_ (taskDetail tasks) $ filterTasks fs tasks

        Right ("projects", [], "") ->
          printProjectCounts tasks

        Left errmsg ->
          putStrLn errmsg

