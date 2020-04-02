{-# LANGUAGE DeriveGeneric #-}

module Task
  ( Desc
  , ID
  , Tasks
  , Actions
  , Task (..)
  , Action (..)
  , uuidExists
  , nextUuid
  , addExactTask
  , deleteTask
  , printTask
  , printTasks
  , applyAction
  , applyActions
  , stringToExact
  ) where

import GHC.Generics (Generic)
import Data.Binary

type Desc = String
type ID   = Integer

type Tasks = [Task]
type Actions = [Action]

data Task = Task
  { uuid :: ID
  , desc :: Desc
  } deriving (Eq, Show, Generic)

data Action = Exact Task
            | Delete ID
            deriving (Eq, Show, Generic)

instance Binary Task
instance Binary Action

-- apply an action to a list of tasks
applyAction :: Tasks -> Action -> Tasks
applyAction ts (Exact t)  = addExactTask ts t
applyAction ts (Delete i) = deleteTask ts i

applyActions :: Tasks -> Actions -> Tasks
applyActions ts [] = ts
applyActions ts (a:as) = applyActions (applyAction ts a) as

uuidExists :: Tasks -> ID -> Bool
uuidExists ts i = foldr (\t acc -> if uuid t == i then True else acc) False ts

nextUuid :: Tasks -> ID
nextUuid [] = 1
nextUuid ts = head $ dropWhile (uuidExists ts) [1..]

-- helper function, add task with all fields provided
-- will be used when loading tasks from file
addExactTask :: Tasks -> Task -> Tasks
addExactTask ts t = t:ts

-- convert from desc string to an Exact action
stringToExact :: Tasks -> String -> Action
stringToExact ts d =
  Exact Task  { uuid = nextUuid ts
              , desc = d
              }

-- remove task with id from list of tasks
deleteTask :: Tasks -> ID -> Tasks
deleteTask ts i = foldr (\t acc -> if i == uuid t then acc else t:acc) [] ts

-- print task
printTask :: Task -> IO ()
printTask t = do
  putStrLn $ i ++ "\t" ++ d
  where i = show $ uuid t
        d = desc t

-- print tasks
printTasks :: Tasks -> IO ()
printTasks [] = return ()
printTasks ts = mapM_ printTask ts

