(ns policyj.postfix.response)

(defn respond-with
  ([action & { :keys [extra msg] :or {extra {} msg nil}}]
     { :action action :message msg :extra extra }))

(defn- status->string
  "Turns a status which is either a string or a keyword into a string that's suitable to be used in a response string"
  [status]
  (clojure.string/upper-case (name status)))

(defn postfix-response
  "Generates a postfix compatible response for the given response map"
  [response]
  (if (:message response)
    (str "action=" (status->string (:action response)) " " (:message response) "\n\n")
    (str "action=" (status->string (:action response)) "\n\n")))
