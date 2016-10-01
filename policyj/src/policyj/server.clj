(ns policyj.server
  (:require [net.tcp.server :as tcp]
            [policyj.logging.logger :as log]
            [policyj.policies :as policies]))

(defn start
  [address port backlog handler]
  (log/jot :info { :event :policy-server-start :bind_address address :bind_port port })

  (tcp/start
   (tcp/tcp-server
    :host address
    :port port
    ;; that's the highest amount of connections we've seen
    :backlog backlog
    :handler (tcp/wrap-io handler))))
