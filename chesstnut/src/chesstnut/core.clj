(ns chesstnut.core)

(defstruct piece :kind :position :color)

(defmulti possible-moves :kind)
(defmulti attacked-by :kind)
(defmulti attacks :kind)
(defmulti start-postition :kind)

(defn position->coords [position]
  (let [x (first (clojure.string/lower-case position))
        y (second position)]
    [(- (int x) 97) (- (- (int y) 48) 1)]))

(defn coords->position [coords]
  (let [x (char (+ 97 (first coords)))
        y (+ 1 (second coords))]
    (str x y)))

(def make-initial-board
  "Returns a board with all figures at their starting positions"
  []
  [[:rook :white "a1"] [:knight :white "b1"] [:bishop :white "c1"] [:queen :white "d1"] [:king :white "e1"] [:bishop :white "f1"] [:knight :white "g1"] [:rook :white "h1"]
   [:rook :black "a8"] [:knight :black "b8"] [:bishop :black "c8"] [:queen :black "d8"] [:king :black "e8"] [:bishop :black "f8"] [:knight :black "g8"] [:rook :black "h8"]])
