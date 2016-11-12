(ns net-protocols.examples.console-server.server
  (:refer-clojure :exclude [read-line])
  (:require [clojure.java.io :as io])
  (:import  (java.net ServerSocket)))


(def users (atom { "bob" "bob"}))

(defn read-line [in]
  (.readLine in))

(defn write-line [out line]
  (doto out
    (.write line)
    (.write "\n")
    .flush))

(defn write-string [out data]
  (doto out
    (.write data)
    .flush))

(defn return [str & { :keys [continue]  :or {continue true} }]
  [str continue])

(defmulti handle-command (fn[command & _] (keyword command)))

(defmethod handle-command :exit [& _]
  (return (str "bye bye\n") :continue false))

(defmethod handle-command :version [& _]
  (return (str "Version: 1.0.0\n")))

(defmethod handle-command :default [& _]
  (return (str "Sorry. Command not understood.\n")))

(defn challenge [reader writer prompt]
  (write-string writer prompt)
  (read-line reader))

(defn try-authentication [in out]
  (let [user     (challenge in out "Username: ")
        password (challenge in out "Password: ")]
    (= password (get @users user))))

(defn authenticate [in out]
  (while (not (try-authentication in out))
    (write-line "Authentication failed")))

(defn banner [out]
  (-> out
    (write-line "Greetings. This is an example console server")
    (write-line "Type 'exit' to close the connection.")))

(defn process-command [reader writer]
  (let [command (challenge reader writer "console>> ")
        [answer continue?] (handle-command command)]
    (write-string writer answer)
    continue?))

(defn process-client [socket]
  (println "Processing new client ...")
  (with-open [socket socket
              reader (io/reader socket)
              writer (io/writer socket)]
    (try
      (banner writer)
      (authenticate reader writer)
      (while (process-command reader writer))
      (catch Exception _ (println "Exception in client. Closing connection")))))

(defmacro forever [& body]
  `(while true ~@body))

(defn start-server [port]
  (with-open [server-sock (ServerSocket. port)]
    (forever
      (let [sock (.accept server-sock)]
        (future (process-client sock))))))

(defn main []
  (println "Starting server on port 12345")
  (start-server 12345))
