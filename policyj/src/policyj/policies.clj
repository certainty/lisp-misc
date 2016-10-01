(ns policyj.policies
  (:require [policyj.config :as cfg]
            [policyj.policies.pass-through :as p-passthrough]
            [policyj.postfix.request :as req]
            [policyj.postfix.response :as resp]
            [policyj.statistics.collector :as stats]
            [clj-time.core :as t]
            [policyj.logging.logger :as log]))

(declare write-response)

(def session-count (atom 0))
(def ^:dynamic *current-request* {})
(def ^:dynamic *current-request-id* nil)


(defn- rescue-dunno
  "Attempts to write the dunno response to output.
   It will catch all and log all exceptions.
  "
  [output request]
  (try
    (write-response output (resp/respond-with :dunno :extra { :policy-id :rescue }) { :extra { :request request } } )
    (catch Exception ex
      (log/jot :error "error during response to postfix" ex))))

(defn- run-policy
  "Runs a policy, catching exceptions.
   A policy is a map that has a name and a handler key.
   The handler should be a function that can be applied to a request object
   If an error accures the error-strategy parameter determines what todo.

   It has to be one of:
   :continue - This means the invocation returns dunno
   :abort    - This means the invocation aborts the pipeline (returning nil)
   In any case the exception will be logged.

   The function returns a response when the policy matched or nil otherwise

   > (run-policy { :name \"test\" :handler (fn [req] (res/respond-with :dunno)) } cfg req :on-error :continue)
  "
  [policy config request & { :keys  [on-error] :or [on-error :continue] } ]
  (try
    ((:handler policy) request config)
    (catch Exception ex
      (log/jot :error  { :event :run-policy, :policy (:name policy), :recover-strategy on-error } ex)
      (when (= on-error :continue)
        (resp/respond-with :dunno :extra { :policy-id :rescue :error (.getMessage ex)})))))

(defn- apply-policy
  [policy config request]
  (let [policy-config (cfg/get config [(policy :id)] {})]
    (run-policy policy policy-config request :on-error :continue)))

(defn- compile-pipeline
  "This function compiles a policy-pipeline from the given vector of policies.
   The pipeline will invoke the policies in order.
   A policy is a map with the following structure:
   :name    -> the name of th policy
   :handler -> a one-argument function that accepts a request-map and returns a response or nil
   It accepts an optional keyword argument :noop, which will make the pipeline
   always return the value of :noop. It will however log the policy that has matched.

   > (compile-pipeline [policy/whitelist,policy/blacklist] cfg)
   > (compile-pipeline [policy/whitelist,policy/blacklist] cfg :noop (resp/respond-with :dunno :msg \"Noop\"))
  "
  [policy-arguments config & { :keys [noop] :or [noop false] }]
  (let [policies (concat policy-arguments [p-passthrough/policy])]
    (log/jot :debug { :event :compile-policy-pipeline, :activated-policies  (map :name policies)})

    (fn [request]
      (loop [policies policies]
        (when policies
          (let [current-policy (first policies)
                response       (apply-policy current-policy config request)]
            (if-not response
              (recur (rest policies))
              (do
                (log/jot :debug { :event :policy-selected :policy-name (:name current-policy) :policy-id (:id current-policy) })
                (if noop
                  (do
                    (log/jot :debug { :event :noop, :action (:action noop), :original-action (:action response)})
                    noop)
                  response)))))))))

(defn create-pipeline
  "Creates the pipeline of policies that shall be applied in each request.

   > (create-pipeline [policy/whitelist policy/blacklist])
  "
  [ policies config & args ]
  (apply compile-pipeline policies config args))

(defn inc-session-count! []
  (swap! session-count inc))

(defn dec-session-count! []
  (swap! session-count dec))

(defn write-response [output response  & [extra]]
  (log/jot :info { :event :postfix-response, :action (:action response), :policy-data (:extra response) :extra extra})
  (.append output (resp/postfix-response response)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn compile-handler
  "Creates a handler function that is suitable to be used with
   a reader and a writer. It will read the request from input pass it through the
   policy pipeline and finally write the response to the writer
  "
  [ policies config & { :keys [noop] :or [noop false]} ]
  (let [policy-pipeline (create-pipeline policies config :noop (and noop (resp/respond-with :dunno :extra { :policy-id :noop })))]

    (fn [input output]
      (try
        (let [session-id (inc-session-count!)
              handler-start-time (t/now)]
          (.setName (Thread/currentThread) (str "session-" session-id))

          (let [request (req/request-map input)]
            (binding [*current-request* request
                      log/*current-log-merge-data* {:threads session-id
                                                    :request-id (uuid),
                                                    :client-address (:client_address request)
                                                    :sender (:sender request)
                                                    :recipient (:recipient request)}]
              (try
                (binding [log/*current-log-merge-data* { :request-id (uuid) }]
                  (log/jot :debug { :event :client-request, :request request}))

                (let [response (policy-pipeline request)
                      runtime (t/in-millis (t/interval handler-start-time (t/now)))]
                  (write-response output response { :runtime-ms runtime })
                  (stats/update response session-id))
                (catch Exception ex
                  (log/jot :error { :event :client-request })
                  (rescue-dunno output *current-request*))))))
        (finally
          (dec-session-count!))))))
