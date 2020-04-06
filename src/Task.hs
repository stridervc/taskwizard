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
  } deriving (Eq)

data Action = Add String
            | Delete ID
            | Done ID
            deriving (Eq)

-- custom show for task, used for saving
instance Show Task where
  show t = "uid:"++i++" done:"++isd ++ " " ++ desc t
    where i   = show $ uid t
          isd = show $ isdone t

instance Show Action where
  show (Add s)    = "add " ++ s
  show (Delete i) = "delete " ++ show i
  show (Done i)   = "done " ++ show i

-- create a new, empty task
newTask :: ID -> Task
newTask i =
  Task  { uid = i
        , isdone = False
        , desc = ""
        }

-- parse and add a task to tasks
addTask :: Tasks -> String -> Tasks
addTask ts s = t:ts
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

parseAction :: String -> Action
parseAction s
  | cmd == "add"    = Add rest
  | cmd == "delete" = Delete $ read rest
  | cmd == "done"   = Done $ read rest
  where cmd  = head $ words s
        rest = unwords $ tail $ words s

parseAddAction :: Tasks -> String -> Action
parseAddAction ts s = Add $ show t
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

parseExactAction :: Tasks -> String -> Either String Action
parseExactAction ts s
  | cmd == "add"    = Right $ parseAddAction ts rest
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
applyAction ts (Add s)  = addTask ts s
applyAction ts (Delete i) = deleteTask ts i
applyAction ts (Done i)   = doTask ts i

applyActions :: Tasks -> Actions -> Tasks
applyActions = foldl applyAction 

tasksFromActions :: Actions -> Tasks
tasksFromActions = applyActions []

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

-- dump actions to list of strings, for saving to file
dumpActions :: Actions -> [String]
dumpActions = map show

-- construct actions from list of strings, for loading from file
unDumpActions :: [String] -> Actions
unDumpActions = map parseAction

