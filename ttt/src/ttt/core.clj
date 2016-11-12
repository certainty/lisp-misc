(ns ttt.core)
(declare game-state generate-successor-states generate-successor-boards)

;; the board
;; A board is represented by a vector of nine elements
;; Each cell in this vector can have one of three values
;; * 1   - Occupied by Player X
;; * -1  - Occupied by Player O
;; * nil - Free

(defn game-state
  "Creates a new gamestate by expanding the game tree one level"
  [player board]
  {
   :player player
   :board  board
   :successors (generate-successor-states player board)
   })

(defn generate-successor-states
  "Generates next game-states. Each game states effectivly represents
   the result of a move, so you can think of this function as generating
   moves
  "
  [player board]
  (let [next-player (* -1 player)]
    (map
     (partial game-state next-player)
     (generate-successor-boards next-player board))))


(defn generate-successor-boards
  "Generates all successor boards. It does this
   simply by occupying each free cell accordingly
   Returs a seq of all boards resulting from the given board."
  [player board]
  (let [indexed-board (map-indexed vector board)
        nil-pos (map first (filter #(nil? (second %)) indexed-board))]
    (map #(assoc board % player) nil-pos)))

(defn initial-board []
  (vec (repeat 9 nil)))
