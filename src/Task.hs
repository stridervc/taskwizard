module Task
  ( Desc
  , ID
  , Task (..)
  , Action
  ) where

type Desc = String
type ID   = Integer

data Task = Task
  { id :: ID
  , desc :: Desc
  }

data Action = Add Desc
            | Done ID
