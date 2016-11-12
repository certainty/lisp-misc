(ns net-protocols.streams.base
  (:refer-clojure :exclude [read flush]))

(defprotocol Stream
  (read  [this timeout] "Reads a single character from the given stream")
  (write [this data] "Writes the given data to the stream")
  (flush [this] "Flush the stream")
  (connected? [this] "Is the stream still connected?")
  (close [this] "Closes the stream"))
