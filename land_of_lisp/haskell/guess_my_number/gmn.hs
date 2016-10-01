import Data.Bits

0data GameCommand = Correct | Smaller | Greater
type GameState = (Integer,Integer)
type Guess = Integer

initialGameState = (1,100)

guessNumber :: GameState -> Guess
-- arithmetic shift by minus one just halfes the number without creating a fraction
guessNumber (lower,upper) = shift (lower + upper) (-1)

smaller :: GameState -> GameState
smaller (l,u) = let newUpper = (guessNumber (l,u)) - 1
             in (l,newUpper)

bigger  :: GameState -> GameState
bigger (l,u) = let newLower = (guessNumber (l,u)) + 1
            in (newLower,u)

readCommand :: IO GameCommand
readCommand = do
  putStrLn "Judge my guess: ( c = correct, s = smaller, g = greater )"
  cmd <- getLine
  case cmd of
    "c" -> return Correct
    "s" -> return Smaller
    "g" -> return Greater
    _ ->  readCommand

turn :: GameState -> Bool -> IO ()
turn _ True = do
  putStrLn "Yay!!"

turn gs False = do
  putStrLn ("My guess is: " ++ show (guessNumber gs))
  cmd <- readCommand
  case cmd of
    Correct -> turn gs True
    Smaller -> turn (smaller gs) False
    Greater -> turn (bigger gs) False

main :: IO ()
main = do
  putStrLn("Please imagine a number between " ++ (show (fst initialGameState)) ++ " and " ++ (show (snd initialGameState)))
  turn initialGameState False
