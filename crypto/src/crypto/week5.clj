(ns crypto.week5
  (import java.math.BigInteger)
  (:gen-class :main true))

(def P (biginteger 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171))
(def G (biginteger 11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568))
(def H (biginteger 3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333))

(defn mod-mul [^java.math.BigInteger a ^java.math.BigInteger b ^java.math.BigInteger m]
  (.mod (.multiply a b) m))

(defn mod-add [^java.math.BigInteger a ^java.math.BigInteger b ^java.math.BigInteger m]
  (.mod (.add a b) m))

;; we need this to create proper ranges of biginteger
;; since the default range function just uses + to increment the number
;; which will be a BigInt.
;; this is an adaption of the code of clojure.core/range which is optimized
;; in terms of chunks.
;; the more straight forward definition could be
;; (defn big-range [start end]
;;   (let [step (biginteger 1)]
;;     (take (.add end step) (iterate #(.add %1 step) start))))
(defn big-range
  ([start end]
     (let [step (biginteger 1)]
       (lazy-seq
        (let [b (chunk-buffer 32)
              comp (cond (or (zero? step) (= start end)) not=
                         (pos? step) <
                         (neg? step) >)]
          (loop [i start]
            (if (and (< (count b) 32)
                     (comp i end))
              (do
                (chunk-append b i)
                (recur (.add i step)))
              (chunk-cons (chunk b)
                          (when (comp i end)
                            (big-range i end))))))))))


(defn gen-exp-table [n-range]
  (let [ginverse (.modInverse G P)]
    (letfn [(fill-table [[table,h] e]
              [(assoc table h e), (mod-mul h ginverse P)])]
      (first (reduce fill-table [{},H] n-range)))))

;; ;; this is a neat definition but doesn't perform as good as the one above
(defn gen-exp-table-neater [n-range]
  (let [ginverse (.modInverse G P)
        keys (iterate #(mod-mul %1 ginverse P) H)]
    (into {} (map vector keys n-range))))

(defn lookup-exp-strict [table n n-range]
  (let [exp (.modPow G n P)]
    (loop [rng n-range key (biginteger 1)]
      (if-let [match (get table key)]
        (mod-add (mod-mul (first rng) n P) match P)
        (when-not (empty? rng)
          (recur (rest rng) (mod-mul key exp P)))))))


;; lazy version of lookup
;; this is actually a neat thing as it reads nicely
;; but it doesn't perform so well. It it's roughly twice as slow
;; as the strict version
(defn all-matches [table exp n-range]
  (let [keys       (iterate #(mod-mul %1 exp P) (biginteger 1))
        all-values (map #(vector (get table %1) %2) keys n-range)]
    (filter (comp identity first) all-values)))

(defn lookup-exp-lazy [table n n-range]
  (let [exp (.modPow G n P)]
    (when-let [match (first (all-matches table exp n-range))]
      (mod-add (mod-mul (second match) n P) (first match) P))))


;; main
(def n (.pow (biginteger 2) (biginteger 20)))
(def n-range (doall (big-range (biginteger 0) n)))

(defn run [lookup]
  (lookup (gen-exp-table n-range) n n-range))

;; (run lookup-exp-lazy)
;; (run lookup-exp-strict)

(defn run-timed [lookup]
  (let [_ (println "Generating table")
        table (time (gen-exp-table-neater n-range))]
    (println "lookup")
    (println (time (lookup table n n-range)))))

(defn -main []
  ;(time (println (run lookup-exp-strict)))
  (run-timed lookup-exp-lazy))
