(ns lol.wizard
  (:use lol.gameloop))


;; the pure part :)
(def ^:const +world+ {
                      :locations {
                                  :living-room "You are in the living-room. A wizard is snoring loudly on the couch"
                                  :garden "You are in a beautiful garden. There is a well in front of you"
                                  :attic  "You are in the attic. There is a giant welding torch in the corner."
                                  }
                      :paths {
                              :living-room [[:garden :west :door] [:attic :upstairs :ladder]]
                              :garden [[:living-room :east :door]]
                              :attic  [[:living-room :downstairs :ladder]]
                              }
                      :objects {
                                :living-room [:whiskey :bucket]
                                :garden [:chain :frog]
                                }
                      })

(defn describe-location [location world]
  ((world :locations) location))

(defn describe-path [edge]
  (let [e (map name edge)]
    (str "There is a " (first e) " going " (second e) " from here")))

(defn describe-paths [location world]
  (let [paths (world :paths)
        edges (paths location)]
    (map describe-path edges)))

(defn objects-at [location world]
  (let [objects (world :objects)]
    (objects location)))

(defn describe-object [obj]
  (str "You see a " (name obj) " on the floor"))

(defn describe-objects [location world]
  (map describe-object (objects-at location world)))

(defn perceive-environment [location world]
  {
   :location (describe-location location world)
   :paths    (describe-paths location world)
   :objects  (describe-objects location world)
   })

(defn environment-display [env]
  (str (env :location) "\n"
       (clojure.string/join "\n" (env :paths))
       "\n"
       (clojure.string/join "\n" (env :objects))))

(defn walk
  "Returns two values. The first indicates the success/failure of the move and the second is the new game-state
   resulting from the walk
  "
  [game-state direction]
  (let [current-location (game-state :current-location)
        world            (game-state :world)
        paths            ((world :paths) current-location)
        next (first (filter (fn [e] (= direction (second e))) paths))]
    (if next
      [true  (assoc game-state :current-location (first next))]
      [false game-state])))

(defn perceive-current-environment [game-state]
  (perceive-environment (game-state :current-location) (game-state :world)))

(defn pickup [game-state object]
  (let [current-location (game-state :current-location)
        world (game-state :world)
        inventory (game-state :inventory)]
    (if (some #{object} (objects-at current-location world))
      [true (assoc game-state :inventory (conj inventory object))]
      [false game-state])))


;; handlers

(defn turn [game-state output]
  {
   :output (if (vector? output) output (vector output))
   :continue true
   :game-state game-state
   })

(defn handle-help [game-state command-map args]
  (turn game-state ["Available commands are:" (clojure.string/join "\n" (map name (keys command-map)))]))

(defn handle-quit [game-state _ _]
  {
   :output ["Good bye"]
   :game-state game-state
   :continue false
   })

(defn handle-look [game-state cmd-map args]
  (turn game-state (environment-display (perceive-current-environment game-state))))

(defn handle-walk [game-state _ args]
  (let [[success gs] (walk game-state (first args))]
    (if success
      (turn gs (environment-display (perceive-current-environment game-state)))
      (turn gs "You can not get there"))))

(defn handle-pickup [game-state _ args]
  (let [[success gs] (pickup game-state (first args))]
    (if success
      (turn gs (str "You are now carrying: " (name (first args))))
      (turn gs "You can not get that"))))

(defn handle-inventory [game-state _ args]
  (let [inventory (game-state :inventory)]
    (if inventory
      (turn game-state (str "You are carrying: " (unlines  (map name inventory))))
      (turn game-state "You don't carry anything"))))

(defn unlines [col]
  (clojure.string/join "\n" col))

;; let us now enter the cruel world of io
(defn dispatch-command [game-state command-map]
  (print "> ")
  (let [cmd (map keyword (clojure.string/split (read-line) #"\s+"))
        handler (command-map (first cmd))]
    (if handler
      (let [res (handler game-state command-map (rest cmd))]
        (when (res :output)
          (println (unlines (res :output))))
        [(res :game-state) (res :continue)])
      (do
        (println "I do not know that command")
        (recur game-state command-map)))))

(defn game-loop-handler [game-state]
  (dispatch-command game-state {
                                :help handle-help
                                :quit handle-quit
                                :look handle-look
                                :walk handle-walk
                                :pickup handle-pickup
                                :inventory handle-inventory}))

(defn main []
  (game-loop { :current-location :living-room :world +world+} true game-loop-handler))
