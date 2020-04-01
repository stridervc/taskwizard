module Task
  ( Desc
  , ID
  , Task (..)
  , Action
  , taskFromString
  , uuidExists
  , nextUuid
  , addTask
  ) where

type Desc = String
type ID   = Integer

data Task = Task
  { uuid :: ID
  , desc :: Desc
  } deriving (Eq, Show)

data Action = Add Desc
            | Done ID

taskFromString :: ID -> String -> Maybe Task
taskFromString _ [] = Nothing
taskFromString x s = Just Task
  { uuid = x
  , desc = s
  }

uuidExists :: [Task] -> ID -> Bool
uuidExists ts i = foldr (\x acc -> if uuid x == i then True else acc) False ts

nextUuid :: [Task] -> ID
nextUuid [] = 1
nextUuid ts = head $ dropWhile (uuidExists ts) [1..]

addTask :: [Task] -> Desc -> [Task]
addTask ts d = t:ts
  where t = Task  { uuid = nextUuid ts
                  , desc = d
                  }

