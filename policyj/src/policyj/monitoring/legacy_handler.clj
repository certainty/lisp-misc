(ns policyj.monitoring.legacy_handler
  (:require
   [policyj.statistics.collector :as stats]
   [policyj.monitoring.errors :as errors]
   [policyj.logging.logger :as log]))

;; this is the handler function that is compatible with
;; the monitoring interface of the policyd and basically
;; delivers the same values. This is here so that we don't
;; have to write a new check for the poliyj now.
;; we will probably some time in the future

(defn now [] (quot (System/currentTimeMillis) 1000))

;; uptime=3282820
;; max_threads=4
;; total_connects=574
;; total_rejects=292
;; total_deferrs=0
;; total_deferred_returners=0
;; total_dunnos=276
;; dnswl_hits=0
;; b.barracudacentral.org_hits=6
;; ix.dnsbl.manitu.net_hits=1
(defn formatted-statistics []
  (let [uptime      (- (now) (@stats/statistics :boot-time))
        max-threads (@stats/statistics :max-threads)
        total       (@stats/statistics :total-connections)
        rejects     (@stats/statistics :rejected-connections)
        dunnos      (- (@stats/statistics :total-connections) (@stats/statistics :rejected-connections))
        lists       (@stats/statistics :dns-blacklisted)
        barracuda   (get lists "b.barracudacentral.org" 0)
        manitu      (get lists "ix.dnsbl.manitu.net" 0)]
    (str "uptime=" uptime "\n"
         "max_threads=" max-threads "\n"
         "total_connects=" total "\n"
         "total_rejects="  rejects "\n"
         "total_deferrs=0\n"
         "total_deferred_returners=0\n"
         "total_dunnos="   dunnos "\n"
         "dnswl_hits=0\n"
         "b.barracudacentral.org_hits=" barracuda "\n"
         "ix.dnsbl.manitu.net_hits=" manitu)))


(defn- map->string [m]
  (clojure.string/join " " (map (fn [e] (clojure.string/join ": " (map name e))) m)))

(defn format-errors [errors]
  (let [fst (comp name first)]
    (clojure.string/join "\n"
                         (map (fn [pair]
                                (str (fst pair) ": " (map->string (second pair))))
                              errors))))

(defn formatted-errors []
  (if (errors/errors?)
    (str "yes\n" (format-errors (errors/errors)))
    "no"))

(defn handler [input]
  (try
    (let [command (clojure.string/trim input)]
      (cond
       (= command "stats") (str (formatted-statistics) "\n\n")
       (= command "reset") (do (stats/reset-statistics!) (errors/clear-errors!) "ok\n\n")
       (= command "errors") (str (formatted-errors) "\n\n")
       :else "Error: Command not understood\n\n"))
    (catch Exception ex
      (log/jot :error { :event :monitoring-handler } ex))))
