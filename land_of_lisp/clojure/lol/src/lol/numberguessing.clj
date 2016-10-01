(ns lol.numberguessing
  (:use lol.gameloop))


;; pure part
(def initial-game-state [1,100])
(def lower-limit first)
(def upper-limit second)

(defn guess-number [gs]
  (let [lower (lower-limit gs)
        upper (upper-limit gs)]
    (bit-shift-right (+ lower upper) 1)))

(defn smaller [gs]
  [(lower-limit gs) (- (guess-number gs) 1)])

(defn greater [gs]
  [(+ (guess-number gs) 1) (upper-limit gs)])


;; io part
(defn read-command []
  (println "Judge my guess: ( c = correct, s=smaller , g=greater )")
  (case (clojure.string/lower-case (read-line))
    "c" :correct
    "s" :smaller
    "g" :greater
    (read-command)))

(defn game-loop-handler [game-state]
  (println "My guess is: " (guess-number game-state))
  (case (read-command)
    :correct (do (println "Yay!!") [game-state false])
    :smaller [(smaller game-state) true]
    :greater [(greater game-state) true]))

(defn main []
  (game-loop initial-game-state true game-loop-handler))
