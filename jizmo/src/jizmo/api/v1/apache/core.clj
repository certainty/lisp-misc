(ns jizmo.api.v1.apache.core
  (:require [jizmo.config :as config]
            [jizmo.api.v1.apache.vhost_config_file :as vhost]
            [jizmo.foundation :as foundation]
            [clojure.tools.logging :as log]))

(defn config-check [config] true)

(defn error [message]
  { :status 400, :message message})

(defn stage-vhost
  "Creates a new vhost for the given domain.
   If a vhost for that domain already exsists, it will be replaced.
  "
  [domain params]
  (let [vhosts-dir (config/config-get [:apache :vhosts_directory])
        staging-file-name (str (vhost/derive-config-filename vhosts-dir domain) ".staging")
        staging-path (str vhosts-dir "/" staging-file-name)]
    (spit staging-path (vhost/render-vhost params))
    (log/info (str "Staged " domain " to " staging-path))
    staging-path))

(defn vhost-valid?
  "This function verifiest that the generated vhost doesn't contain
   errors by running a config check. The command to verify the configuration
   must be configured with under the following key: :apache => :commands => :check
  "
  [path]
  (let [check-command (config/config-get [:apache :commands :check] "/bin/true")
        command (format check-command path)]
    (log/info (str "Executing command: " command))
    (let [result (foundation/execute-command command)]
      (if (zero? (result :exit))
        (log/info "vhost is valid")
        (log/error (str "The vhost is not valid. Config-check returned with exit-code: " (result :exit)))))))

(defn promote-to-production
  "Removes the .staging extension"
  [staging-path]
  (let [production-path (clojure.string/replace staging-path #"\.staging" "")]
    (log/info (str "Promoting " staging-path " to production"))
    (.rename (clojure.java.io/file staging-path) production-path)))

(defn reload-service
  "Reloads the apache service.
   The command to reload the apache must be configured as:
   :apache => :commands => :reload
   A typical config would look like this:
   {
     :apache {
       :commands {
          :reload \"/etc/init.d/apache reload\"
       }
     }
   }
  "
  []
  (let [reload-command (config/config-get [:apache :commands :reload] "/bin/true")]
    (log/info (str "Reloading apache with: " reload-command))
    (foundation/execute-command reload-command)))

(defn create-vhost
  "Creates the vhost and reloads the server if everything works as expected.
   This function will perform a config-check before it actually reloads the
   server, so that we can be quite sure that the reload will not render
   the service unavailable.

   On success it returns the following
   status: 201
   body: { config: basename_of_config_file }

   On error it returns the following
   status: 400
   message: An error message explaining the error
  "
  [{ params :params } cfg]
  (let [domain (params :domain)
        staging-path (stage-vhost domain {})]
    { :status 200 :body cfg }
    ;; (if (vhost-valid? staging-path)
    ;;   (let [production-path (promote-to-production staging-path)]
    ;;     (reload-service)
    ;;     { :status 201, :body { :config domain }})
    ;;   (error "The vhost is not valid"))
    ))
