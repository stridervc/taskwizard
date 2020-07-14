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
  , taskFromID
  , dumpActions
  , unDumpActions
  , refactor
  , tasksToActions
  , parseAddAction
  , modifyTask
  , taskDetail
  , printProjectCounts
  ) where

import Data.List
import Data.Sort
import Data.Time
import Numeric                      (showFFloat)
import System.Console.ANSI
import System.Console.Terminal.Size (height, size, width)

type Desc     = String
type ID       = Integer
type Score    = Float
type Key      = String
type Value    = String
type Width    = Int
type Project  = String
type Priority = Float
type Count    = Int

type Tasks = [Task]
type Actions = [Action]

data Task = Task
  { uid      :: ID
  , desc     :: Desc
  , isdone   :: Bool
  , created  :: UTCTime
  , depends  :: [ID]
  , project  :: Project
  , priority :: Priority
  , started  :: Bool
  , due      :: UTCTime
  } deriving (Eq)

data Action = Add String
            | Delete ID
            | Done ID
            | Start ID
            | Stop ID
            | Modify ID String
            deriving (Eq)

-- list of properties we understand
properties =  [ "uid"
              , "done"
              , "created"
              , "depends"
              , "project"
              , "priority"
              , "started"
              , "due"
              ]

csv :: (Show a) => [a] -> String
csv [] = ""
csv (a:as) = show a ++ comma ++ csv as
  where comma = if length as > 0 then "," else ""

uncsv :: (Read a) => String -> [a]
uncsv "" = []
uncsv s = read f : uncsv rest
  where f = takeWhile (/=',') s
        rest = drop (length f + 1) s

-- Add spaces in between strings and return as a string
spaces :: [String] -> String
spaces [] = ""
spaces (s:ss)
  | s == ""   = spaces ss
  | otherwise = s ++ sp ++ spaces ss
  where sp = if length ss > 0 then " " else ""

-- return "property:value" if value is not default
-- provide a function to convert the value to string
-- used for Show Task
-- property -> value -> function -> default
propertyString :: (Eq a, Show a) => String -> a -> (a -> String) -> a -> String
propertyString ps v f d
  | v == d    = ""
  | otherwise = ps ++ ":" ++ f v

-- custom show for task, used for saving
instance Show Task where
  show t =
    spaces  [ "uid:" ++ i
            , "created:" ++ ct
            , propertyString "done" (isdone t) show False
            , propertyString "started" (started t) show False
            , propertyString "depends" (depends t) csv []
            , propertyString "project" (project t) id ""
            , propertyString "priority" (priority t) show 0
            , "due:" ++ dt
            , desc t
            ]
    where i   = show $ uid t
          ct  = spacesToUnderscores $ show $ created t
          dt  = spacesToUnderscores $ show $ due t

-- return the full name of a property from a shortened form
propertyFromShort :: String -> Maybe Key
propertyFromShort k = if length matches == 1 then Just (head matches) else Nothing
  where matches = [p | p <- properties, k `isPrefixOf` p]

-- check if a string is a known property
isProperty :: String -> Bool
isProperty s = do
  let p = splitProperty s
  case splitProperty s of
    Nothing     -> False
    Just (k,v)  -> do
      case propertyFromShort k of
        Nothing -> False
        Just _  -> True

-- return task with updated priority (priority as string)
-- either a new priority (eg: 100)
-- or an adjustment (eg: +10 or -12)
adjustPriority :: Task -> String -> Task
adjustPriority t np
  | plus      = t { priority = curr + adj }
  | minus     = t { priority = curr - adj }
  | otherwise = t { priority = absol }
  where plus  = fc == '+'
        minus = fc == '-'
        fc    = head np
        adj   = read $ tail np
        absol = read np
        curr  = priority t

-- apply iff property
-- take a task and a string
-- if the string is a known property (eg uid:1)
-- then apply the property to the task
applyProperty :: Task -> String -> Task
applyProperty t s
  | isProperty s  = do
    let Just (k',v) = splitProperty s
    let Just k = propertyFromShort k'
    case k of
      "uid"      -> t {uid = read v}
      "done"     -> t {isdone = read v}
      "created"  -> t {created = read v}
      "depends"  -> t {depends = uncsv v}
      "project"  -> t {project = v}
      "priority" -> adjustPriority t v
      "started " -> t {started = read v}
      "due"      -> t {due = read v}
      otherwise  -> t
  | otherwise = t

instance Show Action where
  show (Add s)      = "add " ++ s
  show (Delete i)   = "delete " ++ show i
  show (Done i)     = "done " ++ show i
  show (Start i)    = "start " ++ show i
  show (Stop i)     = "stop " ++ show i
  show (Modify i s) = "modify " ++ show i ++ " " ++ s

-- create a new, empty task
newTask :: UTCTime -> ID -> Task
newTask ct i =
  Task  { uid       = i
        , isdone    = False
        , desc      = ""
        , created   = ct
        , depends   = []
        , project   = ""
        , priority  = 0
        , started   = False
        , due       = ct
        }

-- parse and add a task to tasks
addTask :: UTCTime -> Tasks -> String -> Tasks
addTask ct ts s = t:ts
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask ct $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

-- replace a task with the modification.
-- first, process properties and tags
-- if anything remains, make it the new desc
modifyTask :: Tasks -> ID -> String -> Tasks
modifyTask ts i s
  | uidExists ts i  = t':ts'
  | otherwise       = ts
  where ts'   = deleteTask ts i
        t''   = taskFromID ts i
        w     = words s
        t'''  = foldl applyProperty t'' w
        d     = unwords $ filter (not . isProperty) w
        t'    = if d == "" then t''' else t''''
        t'''' = t''' { desc = d }

parseAction :: String -> Action
parseAction s
  | cmd == "add"    = Add rest
  | cmd == "delete" = Delete $ read rest
  | cmd == "done"   = Done $ read rest
  | cmd == "start"  = Start $ read rest
  | cmd == "stop"   = Stop $ read rest
  | cmd == "modify" = Modify id rest'
  where cmd   = head $ words s
        rest  = unwords $ tail $ words s
        id    = read $ head $ words rest
        rest' = unwords $ tail $ words rest

parseAddAction :: UTCTime -> Tasks -> String -> Action
parseAddAction ct ts s = Add $ show t
  where t   = foldl applyProperty t' w
        t'  = t'' {desc = d}
        t'' = newTask ct $ nextUid ts
        w   = words s
        d   = unwords $ filter (not . isProperty) w

splitProperty :: String -> Maybe (Key, Value)
splitProperty s
  | ':' `elem` s  = Just (k,v)
  | otherwise     = Nothing
  where k = takeWhile (/= ':') s
        v = underscoresToSpaces $ tail $ dropWhile (/= ':') s

-- apply an action to a list of tasks
applyAction :: UTCTime -> Tasks -> Action -> Tasks
applyAction ct ts (Add s)     = addTask ct ts s
applyAction _ ts (Delete i)   = deleteTask ts i
applyAction _ ts (Done i)     = doTask ts i
applyAction _ ts (Start i)    = startTask ts i
applyAction _ ts (Stop i)     = stopTask ts i
applyAction _ ts (Modify i s) = modifyTask ts i s

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
  where fTask t = t {isdone = True, started = False}

startTask :: Tasks -> ID -> Tasks
startTask ts i = foldr (\t acc -> if i == uid t then (sTask t):acc else t:acc) [] ts
  where sTask t = t {started = True}

stopTask :: Tasks -> ID -> Tasks
stopTask ts i = foldr (\t acc -> if i == uid t then (sTask t):acc else t:acc) [] ts
  where sTask t = t {started = False}

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

-- return a pretty due date time
-- if due = created, then return ""
prettyDue :: UTCTime -> Task -> String
prettyDue now t
  | ct == dt  = ""
  | dt == now = ""
  | otherwise = "some time"
  where ct  = created t
        dt  = due t

-- print task
printTask :: UTCTime -> [Width] -> (Task, Score, Bool) -> IO ()
printTask now ws (t,s,hi) = do
  if started t then
    ansiStarted
  else
    if hi then
      ansiOddRow
    else
      ansiReset

  putStr $ spaces [i,p,d,dues,ss]
  ansiReset
  putStrLn ""

  where i     = ic ++ (padStringLeft iw $ show id)
        p     = pc ++ (padString pw $ project t)
        d     = dc ++ (padString dw $ desc t)
        ss    = sc ++ (padStringLeft sw $ prettyNum s)
        dues  = duec ++ (padString duew $ prettyDue now t)
        id    = uid t
        iw    = ws!!0
        pw    = ws!!1
        dw    = ws!!2
        sw    = ws!!3
        duew  = ws!!4
        ic    = setSGRCode [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White ]
        pc    = setSGRCode [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]
        sc    = setSGRCode [ SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Yellow ]
        dc    = setSGRCode [ SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White ]
        duec  = if (due t > now) then
                  setSGRCode [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]
                else
                  setSGRCode [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red ]

-- print tasks
printTasks :: UTCTime -> Tasks -> IO ()
printTasks _ [] = return ()
printTasks now ts = do
  s <- size   -- console size
  let columns = 5
  let tasks = sorted now $ todo ts
  let maxi = foldl1 max $ map uid tasks
  let iw = length $ show maxi
  let maxs = foldl1 max $ map (score now ts) $ map uid tasks
  let sw = length $ prettyNum maxs
  let mdw = foldl1 max $ map (length . desc) tasks
  let pw = foldl1 max $ map (length . project) tasks
  let duew = foldl1 max $ map (length . prettyDue now) tasks
  let scores = map (score now tasks) $ map uid tasks
  let tsh = zip3 tasks scores $ alternateBool False
  let printem = \dw n -> mapM_ (printTask now [iw,pw,dw,sw,duew]) $ take n tsh

  case s of
    Just s -> do
      let da = width s - iw - sw - pw - duew - (columns-1)
      let n = height s - 3
      if da > mdw + 2 then
        printem (mdw+2) n
      else
        printem da n
    Nothing -> do
      printem 10 10

  ansiReset

  where alternateBool b = b : alternateBool (not b)

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
sorted _ []   = []
sorted now ts = sortBy (compareTasks now ts) ts

-- renumber tasks, starting at ID
renumber :: ID -> Tasks -> Tasks
renumber _ []     = []
renumber i (t:ts) = t {uid = i} : renumber (i+1) ts

-- remove done and deleted tasks
-- keep only tasks that are to do
refactor :: UTCTime -> Tasks -> Tasks
refactor now ts = renumber 1 $ sorted now $ todo ts

-- return Add actions for tasks
tasksToActions :: Tasks -> Actions
tasksToActions []     = []
tasksToActions (t:ts) = Add (show t) : tasksToActions ts

taskFromID :: Tasks -> ID -> Task
taskFromID (t:ts) i
  | uid t == i  = t
  | otherwise   = taskFromID ts i

-- calculate a score for a task
score :: UTCTime -> Tasks -> ID -> Score
score now ts i = times + ds + dc + ps + pri - deps + st + dues
  where diff    = realToFrac $ diffUTCTime now $ created t
        times   = diff / 60 / 60 / 24
        d       = dependants ts t
        ds      = sum $ map (score now ts) d
        dc      = 0.1 * (fromIntegral $ length d)
        t       = taskFromID ts i
        ps      = if project t == "" then 0 else 1
        pri     = priority t
        deps    = 0.1 * (fromIntegral $ length $ depends t)
        st      = if started t then 10 else 0
        duediff = realToFrac $ diffUTCTime now (due t)
        dues    = duediff / 60 / 60 / 24

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

ansiReset :: IO ()
ansiReset = setSGR [ Reset ]

ansiStarted :: IO ()
ansiStarted = setSGR [ SetConsoleIntensity NormalIntensity
                     , SetColor Foreground Vivid White
                     , SetColor Background Dull Magenta
                     ]

ansiOddRow :: IO ()
ansiOddRow = setSGR [ SetConsoleIntensity NormalIntensity
                    , SetColor Foreground Vivid White
                    , SetPaletteColor Background $ xterm24LevelGray 2
                    ]

-- print task detail
taskDetail :: Task -> IO ()
taskDetail t = do
  putStrLn $ "UID     : " ++ show (uid t)
  putStrLn $ "Done    : " ++ show (isdone t)
  putStrLn $ "Started : " ++ show (started t)
  putStrLn $ "Created : " ++ show (created t)
  putStrLn $ "Depends : " ++ show (depends t)
  putStrLn $ "Project : " ++ project t
  putStrLn $ "Priority: " ++ show (priority t)
  putStrLn $ "Due     : " ++ show (due t)
  putStrLn ""
  putStrLn $ desc t
  putStrLn ""

-- return projects and the number of tasks in them
projectCounts :: Tasks -> [(Project,Count)]
projectCounts ts = [(p, count p) | p <- projects]
  where projects  = nub [project t | t <- ts]
        count     = \p -> length [t | t <- ts, project t == p]

printProjectCounts :: Tasks -> IO ()
printProjectCounts ts = do
  mapM_ print' pcs
  where pcs'    = reverse $ sortOn snd $ projectCounts $ todo ts
        pcs     = [pc | pc <- pcs', fst pc /= ""]
        print'  = \(p,c) -> putStrLn $ p ++ "  " ++ show c

