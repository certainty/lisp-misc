(ns monarchy.consumers.archiver.migrations
  (:refer-clojure :exclude [alter drop bigint boolean char double float time])
  (:use (lobos [migration :only [defmigration]] core schema config helpers)))

(defmigration create-samples
  (up [] (create
          (table :samples
            (bigint    :id :auto-inc :primary-key)
            (varchar   :sender-uuid 36 :unique :not-null)
            (check     :sender-uuid (= (length :sender-uuid) 36))
            (timestamp :received_at (default (now)) :not-null))))
  (down [] (drop (table :samples))))
