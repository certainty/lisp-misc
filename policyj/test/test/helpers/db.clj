(ns test.helpers.db
  (:require [clojure.java.jdbc :as j]))

;; update this for your tests
(def credentials
  {:username "username"
   :password "xxxxxxx"
   :port 3306
   :host "db1.example.com"})

(defn emailfilter-db []
  { :subprotocol "mysql"
    :user (credentials :username)
    :password (credentials :password)
    :subname (str "//" (credentials :host) ":" (credentials :port) "/blocklist") })

(defn truncate-tables! [spec & tables]
  (j/with-db-connection [db spec]
    (doseq [table tables]
      (j/execute! db [(str "TRUNCATE TABLE " (j/quoted \` table))]))))

(defn load-fixtures! [spec table & records]
  (j/with-db-connection [db spec]
    (doseq [record records]
      (j/insert! db table record))))
