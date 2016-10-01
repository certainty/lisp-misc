(ns policyj.policies.db.cache
  (:refer-clojure :exclude [empty?])
  (:require [clojure.java.jdbc :as j]
            [clj-time.coerce :as tc]
            [policyj.config :as cfg]
            [policyj.monitoring.errors :as errors]
            [policyj.logging.logger :as log]))

(def ^:private cache (atom {}))

(defn value [] @cache)

(def ^:private select-values (comp vals select-keys))

(defn- database-connection [config]
  {:subprotocol "mysql"
   :user     (config :username)
   :password (config :password)
   :subname (str "//" (config :host) ":" (config :port) "/" (config :database)) })

(defn- cidr-address [record]
  (str (:addr record) "/" (:bits record)))

(defn- extract-values [record]
  (let [expires-at (record :expires_at)]
    (if expires-at
      { :network (cidr-address record) :reason (record :reason) :expires_at (tc/from-sql-time (record :expires_at)) }
      { :network (cidr-address record) :reason (record :reason) :expires_at nil})))

(defn- load-from-authorative-source
  "Queries the authorative source for the given list.
   It will return the set of records on success or an empty set on error.
  "
  [type db-config]
  (let [connection (database-connection db-config)]
    (j/with-db-connection [db connection]
      (j/query db [(str "SELECT addr,bits,reason,expires_at FROM " (j/quoted \` (name type)) "WHERE expires_at IS NULL OR expires_at > NOW()")]
               :row-fn extract-values))))

(defn cache-set! [type value]
  (swap! cache assoc type value))

(defn refresh-cache-for!
  "Tries to refresh the cache for the given type. If it fails for some reason the cache will be set to an empty sequence"
  [type db-config]
  (try
    (try
      (.setName (Thread/currentThread) (str "cache-refresh-" (name type)))

      (let [data (load-from-authorative-source type db-config)]
        (cache-set! type data)
        (log/jot :debug { :event :cache-refresh :name (name type) :authorative-source (select-keys db-config [:database :port :host])}))
      (catch Exception e
        (errors/add-error :cache-refresh { :type type :message (.getMessage e) })
        (log/jot :error { :event :cache-refresh :name (name type) :authorative-source (select-keys db-config [:database :port :host])} e)))
    (catch Exception e
      (errors/add-error :cache-refresh { :type type :message (.getMessage e) })
      (log/jot :error { :event :cache-refresh } e))))

(defn empty? [type]
  (nil? (@cache type)))

(def periodic-refresh-jobs (atom {}))

(defn periodically [callback ms]
  (future (while true (do (Thread/sleep ms) (callback)))))

(defn kill-periodic-job [job]
  (and job (future-cancel job)))

(defn refresh-cache-periodically! [type seconds db-config]
  (let [running-job (@periodic-refresh-jobs type)]
    (when running-job
      (log/jot :debug (str "killing current periodic refresh for" (name type)))
      (kill-periodic-job running-job))
    (do
      (swap! periodic-refresh-jobs assoc type (periodically #(refresh-cache-for! type db-config) (* 1000 seconds)))
      (log/jot :debug (str "Added periodic refresh of cache for" (name type) "every" seconds " seconds")))))
