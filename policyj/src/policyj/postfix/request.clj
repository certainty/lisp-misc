(ns policyj.postfix.request)

(defn- read-request
  "Reads a single request from reader.
   A request is any amount of lines delimited by a single empty line.
  "
  [reader]
  (doall
   (take-while (fn [line] (not (clojure.string/blank? line)))
               (line-seq reader))))

(defn parse-line [line]
  (let [[key value] (map clojure.string/trim (clojure.string/split line #"\s*=\s*"))]
    (if (clojure.string/blank? key)
      (throw (Exception. (str "Invalid line in request: " line)))
      [(keyword key) value])))

(defn- parse-request
  "Parses the array of lines where each line is of the form
  key = value into a map"
  [lines]
  (try
    (if (empty? lines)
      (throw (Exception. "Request was empty"))
      (into {} (map parse-line lines)))))

(defn request-map
  "Reads a request and parses it into a map"
  [reader]
  (parse-request (read-request reader)))
