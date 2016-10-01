(ns policyj.statistics.collector
  (:require
   [policyj.logging.logger :as log]))

;; TODO: cleanup that code

(def boot-time (quot (System/currentTimeMillis) 1000))

(def blank-record { :total-connections 0
                    :rejected-connections 0
                    :max-threads 0
                    :boot-time boot-time
                    :dns-blacklisted {}})

(def statistics (agent blank-record))

(defn reset-statistics! []
  (send statistics (fn [_] blank-record)))

(defn update-totals [response stats]
  (update-in stats [:total-connections] inc))

(defn update-rejects [response stats]
  (if (= (response :action) :reject)
    (update-in stats [:rejected-connections] inc)
    stats))

(defn update-blacklist-stats [response stats]
  (if (and (= (response :action) :reject) (= ((get response :extra {}) :id) :dns-blacklist))
    (let [list-stats (stats :dns-blacklisted)
          service    ((response :extra) :service)]
      (assoc-in stats [:dns-blacklisted] (update-in list-stats [service] (fn [v] (if v (inc v) 1)))))
    stats))

(defn update-sessions [session-id stats]
  (update-in stats [:max-threads] (fn [v] (max v session-id))))

(defn run-update [response session-id stats]
  (try
    (->> stats
         (update-sessions session-id)
         (update-totals response)
         (update-rejects response)
         (update-blacklist-stats response))
    (catch Exception e
      (log/jot :error { :event :statistics-update } e))))

(defn update
  "Updates the response statistics"
  [response session-id]
  ; the session-id is the id of the thread which is incremented/decremented with each thread
  ; so we can use that as an indicator on how many threads are currently active
  (send statistics (partial run-update response session-id)))
