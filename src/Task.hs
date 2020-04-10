module Task
  ( Desc
  , ID
  , Tasks
  , Score
  , Actions
  , Task (..)
  , Action (..)
  , printTasks
  , tasksFromActions
  , parseExactAction
  , dumpActions
  , unDumpActions
  , refactor
  , tasksToActions
  ) where

import Data.Time
import Data.Sort
import System.Console.Terminal.Size (size, width)
import Numeric (showFFloat)

type Desc     = String
type ID       = Integer
type Score    = Float
type Key      = String
type Value    = String
type Width    = Int
type Project  = String

type Tasks = [Task]
type Actions = [Action]

data Task = Task
  { uid     :: ID
  , desc    :: Desc
  , isdone  :: Bool
  , created :: UTCTime
  , depends :: [ID]
  , project :: Project
  } deriving (Eq)

data Action = Add String
            | Delete ID
            | Done ID
            deriving (Eq)

csv :: (Show a) => [a] -> String
csv [] = ""
csv (a:as) = show a ++ comma ++ csv as
  where comma = if length as > 0 then "," else ""

uncsv :: (Read a) => String -> [a]
uncsv "" = []
uncsv s = read f : uncsv rest
  where f = takeWhile (/=',') s
        rest = drop (length f + 1) s

-- custom show for task, used for saving
instance Show Task where
  show t =
    "uid:"++i++" done:"++isd++" created:"++ct++" depends:"++ds++ " project:"++p++" "++desc t
    where i   = show $ uid t
          isd = show $ isdone t
          ct  = spacesToUnderscores $ show $ created t
          ds  = csv $ depends t
          p   = project t

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
        "depends" -> True
        "project" -> True
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
      "depends" -> t {depends = uncsv v}
      "project" -> t {project = v}
      otherwise -> t
  | otherwise     = t

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
        , depends = []
        , project = ""
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

todo :: Tasks -> Tasks
todo = filter (not . isdone)

-- pad string with spaces on the right
padString :: Width -> String -> String
padString w s
  | length s > w  = take w s
  | otherwise     = s ++ replicate (w-length s) ' '

-- pad string with spaces on the left
padStringLeft :: Width -> String -> String
padStringLeft w s
  | ls > w    = take w s
  | otherwise = replicate (w-ls) ' ' ++ s
  where ls = length s

-- print task
printTask :: [Width] -> UTCTime -> Tasks -> Task -> IO ()
printTask ws now ts t = do
  putStrLn $ i ++ " " ++ p ++ " " ++ d ++ " " ++ s
  where i   = padStringLeft iw $ show id
        d   = padString dw $ desc t
        s   = padStringLeft sw $ prettyNum $ score now ts id
        p   = padString pw $ project t
        id  = uid t
        iw  = ws!!0
        pw  = ws!!1
        dw  = ws!!2
        sw  = ws!!3

-- print tasks
printTasks :: UTCTime -> Tasks -> IO ()
printTasks _ [] = return ()
printTasks now ts = do
  s <- size   -- console size
  let columns = 4
  let tasks = sorted now $ todo ts
  let maxi = foldl1 max $ map uid tasks
  let iw = length $ show maxi
  let maxs = foldl1 max $ map (score now ts) $ map uid tasks
  let sw = length $ prettyNum maxs
  let mdw = foldl1 max $ map (length . desc) tasks
  let pw = foldl1 max $ map (length . project) tasks
  let printem = \dw -> mapM_ (printTask [iw,pw,dw,sw] now tasks) tasks

  case s of
    Just w -> do
      let da = width w - iw - sw - pw - (columns-1)
      if da > mdw + 2 then
        printem $ mdw+2
      else
        printem da
    Nothing -> do
      printem 10

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

-- used for sorting
compareTasks :: UTCTime -> Tasks -> Task -> Task -> Ordering
compareTasks now ts t1 t2
  | score1 == score2  = compare (uid t1) (uid t2)
  | otherwise         = compare score2 score1
  where score1  = score now ts i1
        score2  = score now ts i2
        i1      = uid t1
        i2      = uid t2

-- return sorted tasks
sorted :: UTCTime -> Tasks -> Tasks
sorted _ [] = []
sorted now ts = sortBy (compareTasks now ts) ts

-- renumber tasks, starting at ID
renumber :: ID -> Tasks -> Tasks
renumber _ [] = []
renumber i (t:ts) = t {uid = i} : renumber (i+1) ts

-- remove done and deleted tasks
-- keep only tasks that are to do
refactor :: UTCTime -> Tasks -> Tasks
refactor now ts = renumber 1 $ sorted now $ todo ts

-- return Add actions for tasks
tasksToActions :: Tasks -> Actions
tasksToActions [] = []
tasksToActions (t:ts) = Add (show t) : tasksToActions ts

taskFromID :: Tasks -> ID -> Task
taskFromID (t:ts) i
  | uid t == i  = t
  | otherwise   = taskFromID ts i

-- calculate a score for a task
score :: UTCTime -> Tasks -> ID -> Score
score now ts i = times + ds + dc + ps
  where diff  = realToFrac $ diffUTCTime now $ created t
        times = diff / 60 / 60 / 24
        d     = dependants ts t
        ds    = sum $ map (score now ts) d
        dc    = 0.1 * (fromIntegral $ length d)
        t     = taskFromID ts i
        ps    = if project t == "" then 0 else 1

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace s d (w:ws)
  | s == w    = d : replace s d ws
  | otherwise = w : replace s d ws

prettyNum :: (RealFloat a, Show a) => a -> String
prettyNum a = showFFloat (Just 2) a ""

-- return dependants for a task
dependants :: Tasks -> Task -> [ID]
dependants ts t = map uid $ filter (\t -> i `elem` (depends t)) ts
  where i = uid t
