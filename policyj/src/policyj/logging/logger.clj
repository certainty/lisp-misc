(ns policyj.logging.logger
  (:require [clojure.tools.logging :as logging]
            [policyj.logging.formatters.key_value :as kv]
            [clj-time.core :as t]))

(def ^:dynamic *current-log-formatter* kv/format-event)
(def ^:dynamic *current-log-merge-data* {})

(defn stack-trace [exn]
  (mapv (fn [trace]
          {:class (.getClassName trace)
           :file (.getFileName trace)
           :line (.getLineNumber trace)
           :method (.getMethodName trace)})
        (.getStackTrace exn)))

(defn construct-exception-message [exn]
  (when exn
    {:exception
     {:class (class exn)
      :message (if (instance? clojure.lang.ExceptionInfo exn) (.getData exn) (.getMessage exn))
      :localized (.getLocalizedMessage exn)
      ;:stack-trace (stack-trace exn)
      }
     }))

(defn construct-message [message level]
  (merge { :time (t/now)
           :level level
          :message message }
         *current-log-merge-data*))

(defn format-event [event]
  (*current-log-formatter* event))

(defn construct-event [level message & [exn]]
  (let [full-message (construct-message message level)
        exn-message  (construct-exception-message exn)]
    (merge full-message exn-message)))

(defn jot [level message & [exn]]
  (logging/log *ns* level nil (format-event (construct-event level message exn))))
