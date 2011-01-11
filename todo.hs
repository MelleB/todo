{- todo.hs
   -------
   An command line based todo list written in Haskell.
   
   The functionality of this todo list is shamelessly copied from Gina Trapani's 
   todo.txt (http://todotxt.com). 
   
   What I didn't like about todo.txt's implementation is being limited to a
   single todo list. I preferred more related to git, i.e. navigating up the
   directory structure until you've found a todo.txt file.
   
   This is my first "real" experience with Haskell, so if you find any bugs, or
   see any room for code improvement, please (PLEASE!) tell me about it. 
    
   
   Requirements
   ------------
   This application has been tested on Ubuntu 10.10 running GHC 6.12.1
   AFAIK, requirements are limited to GHC 6.12.1 (earlier versions may work too)
   
   
   Usage
   -----
   Usage is pretty much the same as the todotxt's implementation, instead of
   using projects and contexts it simply uses tags to group items. Furtheremore
   I renamed the "do" command to "did" (you're only completing an item once it's
   done right?). A brief overview of the commands:
   
   Add or modify an item
     $ todo add [+tag] Item
     $ todo mod X [+-tag]
   
   E.g. 
     $ todo add +todohs +doc "Writing documentation"
     1. Write documentation (todohs, doc) 
     
     $ todo add +todohs "Work on code"
     2. Work on code (todohs) 
     
     $ todo mod -doc 1
     1. Write documentation (todohs)
    
   Complete an item
     $ todo did 1.
     Completed: 1. Write documentation (todohs)
    
   Clean your history file, removes all completed items and resets item ids
     $ todo vacuum 
    
   List your current items (optionally by tag)
     $ todo ls [tag]
    
   Prioritize or deprioritize 
     $ todo pri 1 A
     Set priority A to "Write documentation".
     $ todo depri 1 A
     Removed priority from "Write documentation".

        
   Each item is a single row in your todo.txt file. Each row has the following
   format:
     id tsadd tsdone <pri> +tag +tag Your todo item here

   The id of an item is defined by the total number of lines in the todo.txt
   file. A history of completed items is kept within the file where id equal to 
   the total number of lines of the document.
   
   "tsadd" and "tsdone" denote timestamps of item addition and completion.
    
-}

import System.Environment (getArgs)
import System.FilePath (FilePath, takeDirectory, pathSeparator)
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import Data.List (intercalate)
import Locale (defaultTimeLocale)
import Data.Time (getCurrentTime, formatTime, FormatTime, UTCTime)
import System.IO
import List (nub)


-- Priority definition
data Priority = PriorityA | PriorityB | PriorityC 
                deriving (Read, Show, Eq)

priorityFromChar :: Char -> Priority
priorityFromChar 'A' = PriorityA
priorityFromChar 'B' = PriorityB
priorityFromChar 'C' = PriorityC
priorityFromChar _   = error "Unknown priority"

priorityToChar :: Priority -> Char
priorityToChar PriorityA = 'A'
priorityToChar PriorityB = 'B'
priorityToChar PriorityC = 'C'

-- TodoItem definition
data TodoItem = TodoItem 
              { todoId :: Int
              , addTime :: Integer
              , completeTime :: Integer
              , priority :: Maybe Priority
              , tags :: [String]
              , description :: String
              } deriving (Show, Eq)

-- Not sure if this is the way to go...
data State = State 
           { args :: [String]
           , items :: [TodoItem]
           , output :: String
           , time :: UTCTime
           } deriving (Show)

-- Execute the action
executeAction :: State -> State
executeAction (State a i o t) = case a of
    ("add":as)      -> addItem (State a i o t)
    ("mod":_)       -> modifyItem (State a i o t)
    ("did":_)       -> completeItem True (State a i o t)
    ("undo":_)      -> completeItem False (State a i o t)
    ("vacuum":_)    -> vacuumList (State a i o t)
    ("ls":_)        -> listItems (State a i o t)
    ("pri":_)       -> prioritizeItem True (State a i o t)
    ("depri":_)     -> prioritizeItem False (State a i o t)
    _               -> (State a i usage t)

-- Add an item
addItem :: State -> State
addItem (State a items o t) 
    | length a < 2 = error "At least two arguments required. See 'help'."
    | True = State a (i:items) output t
    where output = (show (todoId(i))::String) 
                    ++ ". " 
                    ++ desc
                    ++ (if (null tTags)
                        then "" 
                        else " (" ++ (intercalate ", " tTags) ++ ")")
          i = TodoItem { todoId = length items + 1, 
                         addTime = timeToInt t, completeTime = 0,
                         priority = Nothing, tags = tTags, description = desc }
          desc = show (last a) 
          tTags = parseTags (init a)

-- Modify item
modifyItem :: State -> State
modifyItem (State a items o t)
    | length a < 2 = error "At least two arguments required. See 'help'."
    | True = State a (applyOnTodoItem modItem items (read $ (a !! 1)::Int)) output t
    where modItem t = t { tags = nub $ (removeTags $ tags t ++ drop 2 a) }
          output = "Modified todo list item " ++ (a !! 1) ++ "."

-- Mark an item as complete
completeItem :: Bool -> State -> State
completeItem isComplete (State a items o t)
    | length a < 2 = error "At least two arguments required. See 'help'."
    | True = State a (sort (applyOnTodoItem didItem items (read $ (a !! 1)::Int))) output t
    where didItem i = i { completeTime = if isComplete then timeToInt t else 0 }
          sort x | x == [] = []
                 | True = case completeTime(head x) of
                            0 -> head x : (sort (tail x))
                            _ -> (sort (tail x)) ++ [head x]
          output = case isComplete of
                     True  -> "Completed item " ++ (a !! 1) ++ "."
                     False -> "Item " ++ (a !! 1) ++ " is on the todo list again."

-- List incomplete items
listItems :: State -> State
listItems (State a items o t) = (State a items output t)
    where output = case matchedItems of
            [] -> "No items found with tag(s): " ++ (intercalate " " (tail a))
            _  -> matchedItems
          matchedItems = intercalate "\n" 
                        $ map todoItemToString 
                            (filter (matchAllTags (tail a)) items)
          matchAllTags q i = all (`elem` (tags i)) q

-- Vacuums the todo list, i.e. removes all completed items and resets ids
vacuumList :: State -> State
vacuumList (State a i o t) = (State a (vacuumItems i) output t)
    where vacuumItems ls = foldr checkIfComplete [] ls
          checkIfComplete a b = if completeTime(a) == 0 
                                then a { todoId = length b + 1 } : b 
                                else b
          output = "Vacuumed todo list."

prioritizeItem :: Bool -> State -> State
prioritizeItem addPri (State a i o t)
    | addPri && length a < 3 = error "At least three arguments required. See 'help'."
    | length a < 2 = error "At least two arguments required. See 'help'."
    | True = (State a (applyOnTodoItem priItem i (read $ (a !! 1)::Int)) output t)
    where priItem i = i { priority = if addPri 
                                     then Just $ priorityFromChar (head (a !! 2)) 
                                     else Nothing }
          output = case addPri of
          			 True -> "Added priority " ++ (a !! 2) ++ " to item " ++ (a !! 1) ++ "."
          			 False -> "Removed priority from item " ++ (a !! 1) ++ "."


-- Apply a method on a specific todo item
applyOnTodoItem :: (TodoItem -> TodoItem) -> [TodoItem] -> Int -> [TodoItem]
applyOnTodoItem _ [] i = error ("Item " ++ (show i) ++ " not found.")
applyOnTodoItem f (l:ls) i | todoId(l) == i = (f l):ls
                           | todoId(l) /= i = l : (applyOnTodoItem f ls i)
 

-- Parse tags
-- Also removes tags 
parseTags :: [String] -> [String]
parseTags s = filter (\x -> head x == '+') s

-- Remove tags from list starting with a minus
removeTags :: [String] -> [String]
removeTags [] = []
removeTags xx@(x:xs) | head x == '+' && '-':(tail x) `notElem` xx = x : (removeTags xs)
                     | True = removeTags xs


-- Parse a todo item from a string
-- Each line has the following format:
--   id tsadd tsdone <pri> +tag +tag "Your todo item here"
todoItemFromString :: String -> TodoItem
todoItemFromString ln = TodoItem { todoId = tId, addTime = aT, completeTime = cT,
                                   priority = pri, tags = tags, description = desc }
  where (props, desc) = break (=='"') ln
        tId  = read ((words props) !! 0) :: Int
        aT   = read ((words props) !! 1) :: Integer
        cT   = read ((words props) !! 2) :: Integer
        pri  | prop_length < 4  = Nothing
             | prop_length >= 4 = case ((words props) !! 3) of
                                   '<':p:'>':cs -> Just (priorityFromChar p)
                                   _            -> Nothing
        tags | prop_length < 4  = []
             | prop_length < 4 && pri /= Nothing = []
             | True = drop (if pri == Nothing then 3 else 4) $ words props
        prop_length = length (words props) 


-- Convert a todo item to a string       
todoItemToString :: TodoItem -> String
todoItemToString i = (show $ todoId i)
                     ++ '\t':(show $ addTime i)
                     ++ '\t':(show $ completeTime i)
                     ++ (case priority i of
                            Just a  -> ['\t', '<', priorityToChar a,'>']
                            Nothing -> ""
                        )
                     ++ (case null $ tags i of 
                            True -> ""
                            False -> '\t':(intercalate " " $ tags i)
                        )
                     ++ '\t':(description i) 


-- Check every directory on the filepath for a todo.txt file
findTodoFile :: FilePath -> IO FilePath
findTodoFile p = do
  exists <- doesFileExist (p ++ pathSeparator:"todo.txt")
  case exists of
    True ->  return (p ++ pathSeparator:"todo.txt")
    False -> if (p /= takeDirectory p)
             then findTodoFile(takeDirectory p)
             else error "Unable to find a todo.txt file"  

          
-- Print the usage instructions             
usage :: String
usage = "TODO: Add documentation to usage"

timeToInt :: (FormatTime t) => t -> Integer
timeToInt ts = read (formatTime defaultTimeLocale "%s" ts)::Integer
    

-- Execute the program
main :: IO ()
main = do 
      clArgs <- getArgs
      now <- getCurrentTime
      currentPath <- getCurrentDirectory
      todoFile <- findTodoFile currentPath
      handle <- openFile todoFile ReadMode
      input <- hGetContents handle
      state <- return (executeAction (State clArgs (map todoItemFromString $ lines input) [] now))
      putStrLn $ output $ state
      input `seq` hClose handle 
      handle <- openFile todoFile WriteMode
      hPutStr handle $ unlines $ map todoItemToString $ items $ state
      hClose handle
      
