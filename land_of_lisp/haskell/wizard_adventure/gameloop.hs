data GameContinuation gs = Continue { nextGameState :: gs, output :: [String] } |
                           Finish   { output :: [String] }
