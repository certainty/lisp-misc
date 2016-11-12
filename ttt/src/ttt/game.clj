(ns ttt.game
  (:use ttt.core ttt.io))


(defmacro with-gs-vars [[game-state & vars] & body]
  "Example usage: (with-gs-vars (game-state successors vars) (body))"
  (let [evald-game-state-name (gensym)]
    `(let [~evald-game-state-name ~game-state
           ~@(reduce into [] (map (fn [var] `(~var (~(keyword var) ~evald-game-state-name))) vars))]
       ~@body)))

(def parse-int [s]
  (Integer. (re-find #"[0-9]+" s)))

(defn free? [board pos]
  (not (nil? (get board pos))))

(defn occupied? [board pos]
  (not (free? board pos)))

(defn board-full? [board]
  (not-any? nil? board))

(defn win-conditions-met? [board win-condition]
  (let [p1 (board (first win-condition))
        p2 (board (second win-condition))
        p3 (board (last win-condition))]
    (and p1 p2 p3 (= p1 p2 p3))))

(defn winner [board]
  (let [win-conditions '])
  )

(defn select-game-state
  "Selects the game-state that is eqivalent to that move"
  [game-states position]
  (first (filter (fn [gs] (= -1 (get (:board gs) position))) game-states)))

(defn ranking
  "This is really the heart of the algorithm. It ranks
   the possible successors with the minimax algorithm
  "
  [game-state]
  (with-gs-vars [game-state successors player board]
    (let [win (winner board)
          rank-fn (if (x? player) min max)]
      (if win
          win
          (apply rank-fn (remove nil? (map ranking successors)))))))

(defn human-choose-move [game-state]
  (println "Enter your move, human: ")
  (with-gs-vars [game-state successors]
    (let [selection (dec (parse-int (get-input)))]
      (if (occupied? (:board game-state) selection)
        (do (println "That position has already been taken")
            (human-choose-move game-state))
        (select-game-state successors selection)))))

(defn ai-choose-move [game-state]
  (first (sort-by ranking > (:successors game-state))))

(defn turn [current-player game-state]
  (if (= "q" game-state)
    (println "Goodby")
    (do (with-gs-vars [game-state board]
          (display-board board)
          (cond
           (winner? board) (handle-win game-state)
           (draw? board)  (handle-draw game-state)
           true
           (if (= :ai current-player)
             (turn :human (human-choose-move game-state))
             (turn :ai (ai-choose-move game-state))))))))

(defn get-input []
  (clojure.string/trim (read-line)))
