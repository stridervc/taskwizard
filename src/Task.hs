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

type Desc   = String
type ID     = Integer
type Score  = Float
type Key    = String
type Value  = String
type Width  = Int

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
printTask :: [Width] -> UTCTime -> Task -> IO ()
printTask (iw:dw:sw:[]) now t = do
  putStrLn $ i ++ " " ++ d ++ " " ++ s
  where i = padStringLeft iw $ show $ uid t
        d = padString dw $ desc t
        s = padString sw $ prettyNum $ score now t

-- print tasks
printTasks :: UTCTime -> Tasks -> IO ()
printTasks _ [] = return ()
printTasks now ts = do
  s <- size   -- console size
  let maxi = foldl1 max $ map uid ts
  let iw = length $ show maxi
  let maxs = foldl1 max $ map (score now) ts
  let sw = length $ prettyNum maxs
  let mdw = foldl1 max $ map (length . desc) ts

  case s of
    Just w -> do
      let da = width w - iw - sw - 2
      if da > mdw + 2 then
        mapM_ (printTask [iw,mdw+2,sw] now) $ sorted now $ todo ts
      else
        mapM_ (printTask [iw,da,sw] now) $ sorted now $ todo ts
    Nothing -> do
      let dw = 10
      mapM_ (printTask [iw,dw,sw] now) $ sorted now $ todo ts

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
compareTasks :: UTCTime -> Task -> Task -> Ordering
compareTasks now t1 t2
  | score1 == score2  = compare (uid t1) (uid t2)
  | score1 < score2   = GT
  | score1 > score2   = LT
  where score1 = score now t1
        score2 = score now t2

-- return sorted tasks
sorted :: UTCTime -> Tasks -> Tasks
sorted _ [] = []
sorted now ts = sortBy (compareTasks now) ts

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

-- calculate a score for a task
score :: UTCTime -> Task -> Score
score now t = diff / 60 / 60 / 24
  where diff = realToFrac $ diffUTCTime now $ created t

-- round to 2 decimals
round2Dec :: (RealFrac a) => a -> a
round2Dec i = fromIntegral (round (i*100)) / 100

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace s d (w:ws)
  | s == w    = d : replace s d ws
  | otherwise = w : replace s d ws

prettyNum :: (RealFrac a, Show a) => a -> String
prettyNum a = s ++ suff
  where s = replace 'e' '0' $ show $ round2Dec a
        d = (length $ dropWhile (/= '.') s) - 1
        suff = if d == -1 then ".00" else replicate (2-d) '0'

