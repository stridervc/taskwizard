module Task
  ( Desc
  , ID
  , Tasks
  , Actions
  , Task (..)
  , Action (..)
  , printTasks
  , tasksFromActions
  , parseExactAction
  , dumpActions
  , unDumpActions
  ) where

import Data.Time

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
  , created :: UTCTime
  } deriving (Eq)

data Action = Add String
            | Delete ID
            | Done ID
            deriving (Eq)

-- custom show for task, used for saving
instance Show Task where
  show t =
    "uid:"++i++" done:"++isd ++ " created:" ++ ct ++ " " ++ desc t
    where i   = show $ uid t
          isd = show $ isdone t
          ct  = spacesToUnderscores $ show $ created t

instance Show Action where
  show (Add s)    = "add " ++ s
  show (Delete i) = "delete " ++ show i
  show (Done i)   = "done " ++ show i

-- create a new, empty task
newTask :: UTCTime -> ID -> Task
newTask ct i =
  Task  { uid     = i
        , isdone  = False
        , desc    = ""
        , created = ct
        }

-- parse and add a task to tasks
addTask :: UTCTime -> Tasks -> String -> Tasks
addTask ct ts s = t:ts
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask ct $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

parseAction :: String -> Action
parseAction s
  | cmd == "add"    = Add rest
  | cmd == "delete" = Delete $ read rest
  | cmd == "done"   = Done $ read rest
  where cmd  = head $ words s
        rest = unwords $ tail $ words s

parseAddAction :: UTCTime -> Tasks -> String -> Action
parseAddAction ct ts s = Add $ show t
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask ct $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

parseExactAction :: UTCTime -> Tasks -> String -> Either String Action
parseExactAction ct ts s
  | cmd == "add"    = Right $ parseAddAction ct ts rest
  | not exists      = Left $ "Unknown ID: " ++ rest
  | cmd == "delete" = Right $ Delete id
  | cmd == "done"   = Right $ Done id
  | otherwise       = Left $ "Unknown command: " ++ cmd
  where cmd   = head $ words s
        rest  = unwords $ tail $ words s
        id    = read rest
        exists  = uidExists ts id

splitProperty :: String -> Maybe (Key, Value)
splitProperty s
  | ':' `elem` s  = Just (k,v)
  | otherwise     = Nothing
  where k = takeWhile (/= ':') s
        v = underscoresToSpaces $ tail $ dropWhile (/= ':') s

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
        "created" -> True
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
      "created" -> t {created = read v}
      otherwise -> t
  | otherwise     = t

-- apply an action to a list of tasks
applyAction :: UTCTime -> Tasks -> Action -> Tasks
applyAction ct ts (Add s)   = addTask ct ts s
applyAction _ ts (Delete i) = deleteTask ts i
applyAction _ ts (Done i)   = doTask ts i

applyActions :: UTCTime -> Tasks -> Actions -> Tasks
applyActions ct ts as = foldl (applyAction ct) ts as

tasksFromActions :: UTCTime -> Actions -> Tasks
tasksFromActions ct as = applyActions ct [] as

uidExists :: Tasks -> ID -> Bool
uidExists ts i = foldr (\t acc -> if uid t == i then True else acc) False ts

nextUid :: Tasks -> ID
nextUid [] = 1
nextUid ts = head $ dropWhile (uidExists ts) [1..]

-- remove task with id from list of tasks
deleteTask :: Tasks -> ID -> Tasks
deleteTask ts i = foldr (\t acc -> if i == uid t then acc else t:acc) [] ts

-- mark task as done
doTask :: Tasks -> ID -> Tasks
doTask ts i = foldr (\t acc -> if i == uid t then (fTask t):acc else t:acc) [] ts
  where fTask t = t {isdone = True}

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

-- dump actions to list of strings, for saving to file
dumpActions :: Actions -> [String]
dumpActions = map show

-- construct actions from list of strings, for loading from file
unDumpActions :: [String] -> Actions
unDumpActions = map parseAction

spacesToUnderscores :: String -> String
spacesToUnderscores = foldr (\c acc -> if c == ' ' then '_':acc else c:acc) ""

underscoresToSpaces :: String -> String
underscoresToSpaces = foldr (\c acc -> if c == '_' then ' ':acc else c:acc) ""

