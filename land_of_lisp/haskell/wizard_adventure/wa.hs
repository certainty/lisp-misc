{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

type LocationName        = String
type LocationDescription = String
type Direction           = String
type Edge                = (LocationName,Direction,String)

data GameWorld = GameWorld {
      gameLocations :: [(LocationName,LocationDescription)],
      gamePaths     :: [(LocationName,[Edge])],
      gameObjects   :: [(LocationName, [String])]
} deriving (Show)

data Environment = Environment {
     currentLocationDescr :: LocationDescription,
     currentPathDescr     :: [String],
     currentObjectDescr   :: [String]
} deriving (Show)

data GameState = GameState {
     currentLocation :: String,
     world           :: GameWorld,
     inventory       :: [String]
} deriving (Show)


data Turn = Continue {
      turnOutput    :: [String],
      nextGameState :: GameState
    } | Stop {
      turnOutput :: [String]
    } deriving (Show)

type CommandMap = [(String,(GameState -> [String] -> Turn))]

theWorld = GameWorld {
             gameLocations = [("living-room", "You are in the living-room. A wizard is snoring loudly on the couch"),
                              ("garden",      "You are in a beautiful garden. There is a well in front of you"),
                              ("attic",       "You are in the attic. There is a giant welding torch in the corner")],
             gamePaths     = [("living-room", [("garden", "west","door"),("attic", "upstairs","ladder")]),
                              ("garden",      [("living-room", "east", "door")]),
                              ("attic",       [("living-room","downstairs","ladder")])],
             gameObjects   = [("living-room", ["whiskey", "bucket"]),
                              ("garden", ["chain","frog"])]
}


-- all these functions are unsafe in that they assume that the locations passed
-- is does actually exist
locationExists :: LocationName -> GameWorld -> Bool
locationExists loc world = case lookup loc $ gameLocations world of
                             Just _  -> True
                             Nothing -> False

describeLocation :: LocationName -> GameWorld -> LocationDescription
describeLocation loc world = fromJust $ lookup loc $ gameLocations world

describePath :: Edge -> String
describePath (_,to,way) = "There is a " ++ way ++ " going " ++ to ++ " from here."

describePaths :: LocationName -> GameWorld -> [String]
describePaths loc world = case lookup loc $ gamePaths world of
                            Just edge -> (map describePath edge)
                            Nothing   -> []

objectsAt :: LocationName -> GameWorld -> [String]
objectsAt loc world = case lookup loc $ gameObjects world of
                        Just objects -> objects
                        Nothing      -> []

describeObjects :: LocationName -> GameWorld -> [String]
describeObjects loc world = map describeObject $ objectsAt loc world
     where
       describeObject obj = "You see a " ++ obj ++ " on the floor"

perceive :: LocationName -> GameWorld -> Either String Environment
perceive loc world = if locationExists loc world
                     then Right env
                     else Left "There is no such location"
    where
      env = Environment {
              currentLocationDescr = describeLocation loc world,
              currentPathDescr     = describePaths    loc world,
              currentObjectDescr   = describeObjects  loc world
            }

perceiveCurrentEnvironment :: GameState -> Environment
perceiveCurrentEnvironment gs = case perceive loc w of
                                  Right env -> env
    where
       loc   = currentLocation gs
       w     = world gs

environmentDisplay :: Environment -> String
environmentDisplay env = unlines [ currentLocationDescr env,
                                   unlines $ currentPathDescr env,
                                   unlines $ currentObjectDescr env]

walk :: GameState -> Direction -> Either String GameState
walk gs direction = case pathsFrom location currentWorld of
                      (Just paths) -> case find (matchingPath direction) paths of
                                        (Just edge) -> Right gs{ currentLocation = (first edge) }
                                        Nothing     -> Left "You can not go there"
                      Nothing      -> Left "You can not go there"
    where
      location      = currentLocation gs
      currentWorld  = world gs
      matchingPath dir path = dir == second path
      second (_,x,_) = x
      first  (x,_,_) = x
      pathsFrom loc world = lookup loc $ gamePaths world

pickup :: GameState -> String -> Either String GameState
pickup gs object = if object `elem` objectsAt location currentWorld
                   then Right gs{ inventory = object : inv }
                   else Left "You can not get that"
    where
      location     = currentLocation gs
      currentWorld = world gs
      inv          = inventory gs

dropObject :: GameState -> String -> Either String GameState
dropObject gs object = if object `elem` (inventory gs)
                 then Right gs{ inventory = delete object (inventory gs)}
                 else Left "You do not carry that"

continue :: GameState -> [String] -> Turn
continue gs output = Continue {
                        turnOutput    = output,
                        nextGameState = gs
                      }
stop :: [String] -> Turn
stop output = Stop { turnOutput = output }

handleQuit :: GameState -> [String] -> Turn
handleQuit gs args = stop ["Good Bye"]

handleWalk :: GameState -> [String] -> Turn
handleWalk gs args = case walk gs $ head args of
                       Right newState -> continue newState [envInfo newState]
                       Left  msg      -> continue gs [msg]
    where
      envInfo state = environmentDisplay $ perceiveCurrentEnvironment state

handlePickup :: GameState -> [String] -> Turn
handlePickup gs args = let item = head args
                       in case pickup gs $ item of
                            Right newState -> continue newState ["You are now carrying a " ++ item]
                            Left  msg      -> continue gs [msg]

handleDrop :: GameState -> [String] -> Turn
handleDrop gs args = let item = head args
                     in case dropObject gs item of
                          Right newState -> continue newState ["You dumped the " ++ item]
                          Left  msg      -> continue gs [msg]

handleInventory :: GameState -> [String] -> Turn
handleInventory gs args = let inv = inventory gs
                          in
                            continue gs [("You are carrying: " ++ (show inv))]

handleLook :: GameState -> [String] -> Turn
handleLook gs args = continue gs [envInfo]
    where
      envInfo = environmentDisplay $ perceiveCurrentEnvironment gs


-- ok time for io
dispatchCommand :: GameState -> CommandMap -> (IO Turn)
dispatchCommand gs commands = do
   input <- getLine
   let !cmd = words input
   case lookup (head cmd) commands of
     Just handler -> return (handler gs $ tail cmd)
     Nothing      -> unknownCommand
   where
     command inp = head $ lines inp
     unknownCommand = do
       putStrLn "Unknown commmand"
       dispatchCommand gs commands

runGame :: GameState -> CommandMap -> IO ()
runGame gs commands = do
   turn <- dispatchCommand gs commands
   case turn of
     Continue { turnOutput = o, nextGameState = ngs } -> outputAndContinue o ngs
     Stop     { turnOutput = o }                      -> putStrLn $ unlines o
   where
     outputAndContinue output gamestate = do
                        putStrLn $ unlines output
                        runGame gamestate commands


theCommands = [("look", handleLook),
               ("quit", handleQuit),
               ("walk", handleWalk),
               ("pickup", handlePickup),
               ("drop", handleDrop),
               ("inventory", handleInventory)]

initialGameState = GameState {
                     currentLocation = "living-room",
                     world = theWorld,
                     inventory = []
                   }

main = do
  putStrLn $ environmentDisplay $ perceiveCurrentEnvironment initialGameState
  runGame initialGameState theCommands
