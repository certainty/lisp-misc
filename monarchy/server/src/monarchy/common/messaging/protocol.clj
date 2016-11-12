(ns monarchy.common.messaging.protocol)

;; TODO use protocol instead of multimethods for speed?

(defmulti do-publish   (fn [type & args] type))
(defmulti do-subscribe (fn [type & args] type))

(defmethod do-publish   :default [&ignored] (throw (Exception. "Unsupported broker type")))
(defmethod do-subscribe :default [&ignored] (throw (Exception. "Unsupported broker type")))
