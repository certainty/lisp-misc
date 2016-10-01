(ns policyj.monitoring.server
  (:require
   [net.tcp.server :as tcp]
   [policyj.monitoring.legacy_handler :as legacy]
   [policyj.logging.logger :as log]))

(defn wrap-handler [handler]
  (fn [input output]
    (try
      (.setName (Thread/currentThread) "monitoring-handler")
      (log/jot :info { :event :monitoring-new-request })
      (let [command (.readLine input)]
        (.append output (legacy/handler command)))
      (catch Exception ex
        (log/jot :error { :event :monitoring-new-request } ex)))))

;; The monitoring component
(defn start [address port]
  (.setName (Thread/currentThread) "monitoring-server")
  (log/jot :info { :event :monitoring-server-start, :bind_address address, :bind_port port })
  (tcp/start
   (tcp/tcp-server
    :host address
    :port port
    :handler (tcp/wrap-io (wrap-handler legacy/handler)))))
