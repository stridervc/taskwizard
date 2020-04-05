{-# LANGUAGE DeriveGeneric #-}

module Task
  ( Desc
  , ID
  , Tasks
  , Actions
  , Task (..)
  , Action (..)
  , uidExists
  , nextUid
  , addExactTask
  , deleteTask
  , printTask
  , printTasks
  , applyAction
  , applyActions
  , stringToExact
  , parseAddTask
  ) where

import GHC.Generics (Generic)
import Data.Binary

type Desc   = String
type ID     = Integer
type Key    = String
type Value  = String

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

-- custom show for task, used for saving
instance Show Task where
  show t = "uid:"++i++" done:"++isd ++ " " ++ desc t
    where i   = show $ uid t
          isd = show $ isdone t

-- create a new, empty task
newTask :: ID -> Task
newTask i = Task  { uid = i
                  , isdone = False
                  , desc = ""
                  }

-- parse and add a task to tasks
parseAddTask :: Tasks -> String -> Tasks
parseAddTask ts s = t:ts
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

splitProperty :: String -> Maybe (Key, Value)
splitProperty s
  | ':' `elem` s  = Just (k,v)
  | otherwise     = Nothing
  where k = takeWhile (/= ':') s
        v = tail $ dropWhile (/= ':') s

-- check if a string is a known property
isProperty :: String -> Bool
isProperty s = do
  let p = splitProperty s
  case p of
    Nothing -> False
    Just (k,v) -> do
      case k of
        "uid"     -> True
        "done"    -> True
        otherwise -> False

-- apply iff property
-- take a task and a string
-- if the string is a known property (eg uid:1)
-- then apply the property to the task
applyProperty :: Task -> String -> Task
applyProperty t s
  | isProperty s  = do
    let Just (k,v) = splitProperty s
    case k of
      "uid"     -> t {uid = read v}
      "done"    -> t {isdone = read v} 
      otherwise -> t
  | otherwise     = t

-- apply an action to a list of tasks
applyAction :: Tasks -> Action -> Tasks
applyAction ts (Exact t)  = addExactTask ts t
applyAction ts (Delete i) = deleteTask ts i
applyAction ts (Done i)   = doTask ts i

applyActions :: Tasks -> Actions -> Tasks
applyActions = foldl applyAction 

uidExists :: Tasks -> ID -> Bool
uidExists ts i = foldr (\t acc -> if uid t == i then True else acc) False ts

nextUid :: Tasks -> ID
nextUid [] = 1
nextUid ts = head $ dropWhile (uidExists ts) [1..]

-- helper function, add task with all fields provided
-- will be used when loading tasks from file
addExactTask :: Tasks -> Task -> Tasks
addExactTask ts t = t:ts

-- convert from desc string to an Exact action
stringToExact :: Tasks -> String -> Action
stringToExact ts d =
  Exact Task  { uid     = nextUid ts
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

