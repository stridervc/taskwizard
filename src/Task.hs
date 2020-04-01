module Task
  ( Desc
  , ID
  , Task (..)
  , Action
  , uuidExists
  , nextUuid
  , addTask
  , dropTask
  , printTask
  , printTasks
  ) where

type Desc = String
type ID   = Integer

data Task = Task
  { uuid :: ID
  , desc :: Desc
  } deriving (Eq, Show)

data Action = Add Desc
            | Done ID

uuidExists :: [Task] -> ID -> Bool
uuidExists ts i = foldr (\t acc -> if uuid t == i then True else acc) False ts

nextUuid :: [Task] -> ID
nextUuid [] = 1
nextUuid ts = head $ dropWhile (uuidExists ts) [1..]

-- helper function, add task with all fields provided
-- will be used when loading tasks from file
addExactTask :: [Task] -> Task -> [Task]
addExactTask ts t = t:ts

-- add a task to a list of tasks
addTask :: [Task] -> Desc -> [Task]
addTask ts d = t:ts
  where t = Task  { uuid = nextUuid ts
                  , desc = d
                  }

-- remove task with id from list of tasks
dropTask :: [Task] -> ID -> [Task]
dropTask ts i = foldr (\t acc -> if i == uuid t then acc else t:acc) [] ts

-- print task
printTask :: Task -> IO ()
printTask t = do
  putStrLn $ i ++ "\t" ++ d
  where i = show $ uuid t
        d = desc t

-- print tasks
printTasks :: [Task] -> IO ()
printTasks [] = return ()
printTasks ts = mapM_ printTask ts

