module Task
  ( Desc
  , ID
  , Task (..)
  , Action
  , taskFromString
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
