(ns jizmo.api.v1.apache.vhost_config_file
  (:use jizmo.foundation)
  (:require [clojure.tools.logging :as log]))

(def vhost-template
  "
# generated by jizmo at: {{render-time}}

<VirtualHost>
  ServerName {{servername}}
  ServerAlias {{#serveraliases}}{{.}} {{/serveraliases}}
</VirtualHost>" )

(defn render-vhost [params]
  (render-template vhost-template params))

(defn vhost-config-file?
  "Checks if the given path constitutes a vhost-config-file.
   A config file looks like this:
   [0-9]_domain-name.conf
  "
  [path]
  (and
   (.isFile (clojure.java.io/file path))
   (re-find #".*\\.conf$" path)))

(defn numeric-prefix->number [prefix]
  (when (re-find #"\d+" prefix)
    (Integer. prefix)))

(defn number->numeric-prefix [number]
  (if (< number 10)
    (str "0" number)
    (str number)))

(defn extract-prefix [path]
  (let [filename (file-basename path)
        prefix   (second (first (re-seq #"^([0-9]+)_.*" filename)))]
    (when prefix
      (numeric-prefix->number prefix))))

(defn numeric-prefix?
  "Checks if the given path refers to a file that has a numeric prefix.
   A numeric prefix is seperated by an underscore and is the very first
   part of the filename.
  "
  [path]
  (number? (extract-prefix path)))

(defn config-files-in
  "Returns alls apache configfiles in the given directory.
   A file is considered a config-file if vhost-config-file?
   applied to that path returns true
 "
  [dir]
  (filter vhost-config-file? (map #(.getPath %) (file-seq (clojure.java.io/file dir)))))

(defn next-available-prefix
  "Returns the next numeric prefix that is available.
   If there are configfiles already that prefix will be
   1 greater than the greatest prefix
   If there are no configfiles yet, it will return 'base'.
  "
  [paths base]
  (let [prefixes (filter #(not (nil? %)) (map extract-prefix paths))]
    (if (or (nil? prefixes) (empty? prefixes))
      base
      (apply max prefixes))))

(defn derive-config-filename
  "Generates a configuration-filename. It finds the next free prefix
   and combines it with the domain-name such that the result will
   be a valid vhost-config path in the sense of vhost-config-file?
  "
  [directory domain]
  (log/debug (str "Deriving config for " domain " at " directory))
  (let [existing-configs (config-files-in directory)
        prefix  (next-available-prefix existing-configs 10)]
    (str prefix "_" domain ".conf")))
