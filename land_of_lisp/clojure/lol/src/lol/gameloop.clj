(ns lol.gameloop)

(defn game-loop
  "This is our generic game loop.
   It gets passed a game-state a handler function and a flag that indicates if the loop shall continue
  "
  [game-state continue fn]
  (when continue
    (let [[new-state cont] (fn game-state)]
      (recur new-state cont fn))))
