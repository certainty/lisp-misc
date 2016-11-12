(ns ttt.io)

(defn add-position-numbers
  "Label each cell in the board with its position index"
  [board]
  (let [counter (atom 0)]
    (map (fn [cell] (swap! counter inc) (or cell (str @counter))) board)))

(defn rows [board]
  (partition 3 board))

(defn write-cell [cell]
  (cond
   (= cell 1) " X "
   (= cell -1) " 0 "
   true (str " " cell " ")))

(defn write-row [row]
  (str (apply str (interpose \| (map write-cell row))) "\n"))

(defn write-board
  "Returns a prinable representation of the board"
  [board]
  (apply str
         (interpose "------------\n"
           (map write-row (rows (add-position-numbers board))))))

(defn display-board [board]
  (print (write-board board)))
