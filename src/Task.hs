{-# LANGUAGE DeriveGeneric #-}

module Task
  ( Desc
  , ID
  , Tasks
  , Actions
  , Task (..)
  , Action (..)
  , uidExists
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
  { uid     :: ID
  , desc    :: Desc
  , isdone  :: Bool
  } deriving (Eq, Generic)

data Action = Exact Task
            | Delete ID
            | Done ID
            deriving (Eq, Show, Generic)

instance Binary Task
instance Binary Action

-- custom show for task, used for saving and loading
instance Show Task where
  show t = "uid:"++i++" isdone:"++isd ++ " " ++ desc t
    where i   = show $ uid t
          isd = show $ isdone t

-- apply an action to a list of tasks
applyAction :: Tasks -> Action -> Tasks
applyAction ts (Exact t)  = addExactTask ts t
applyAction ts (Delete i) = deleteTask ts i
applyAction ts (Done i)   = doTask ts i

applyActions :: Tasks -> Actions -> Tasks
applyActions = foldl applyAction 

uidExists :: Tasks -> ID -> Bool
uidExists ts i = foldr (\t acc -> if uid t == i then True else acc) False ts

nextUuid :: Tasks -> ID
nextUuid [] = 1
nextUuid ts = head $ dropWhile (uidExists ts) [1..]

-- helper function, add task with all fields provided
-- will be used when loading tasks from file
addExactTask :: Tasks -> Task -> Tasks
addExactTask ts t = t:ts

-- convert from desc string to an Exact action
stringToExact :: Tasks -> String -> Action
stringToExact ts d =
  Exact Task  { uid     = nextUuid ts
              , desc    = d
              , isdone  = False
              }

-- remove task with id from list of tasks
deleteTask :: Tasks -> ID -> Tasks
deleteTask ts i = foldr (\t acc -> if i == uid t then acc else t:acc) [] ts

-- mark task as done
doTask :: Tasks -> ID -> Tasks
doTask ts i = foldr (\t acc -> if i == uid t then (fTask t):acc else t:acc) [] ts
  where fTask t = Task  { uid     = uid t
                        , desc    = desc t
                        , isdone  = True
                        }

-- print task
printTask :: Task -> IO ()
printTask t = do
  putStrLn $ i ++ "\t" ++ d
  where i = show $ uid t
        d = desc t

-- print tasks
printTasks :: Tasks -> IO ()
printTasks [] = return ()
printTasks ts = mapM_ printTask $ filter (not . isdone) ts

