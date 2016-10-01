(ns policyj.policies.db.net-utils)

;; TODO: convert to be more clojurey
;; TODO: add error handling

(defn- pack-octet [addr octet index]
  (bit-or addr (bit-shift-left (bit-and octet 0xff) (* 8 (- 4 index)))))

(defn- address-from-vector [octets]
  (first
   (reduce (fn [accu octet]
             (let [[addr idx] accu]
               [(pack-octet addr octet idx) (+ idx 1)]))
           [0 1]
           octets)))

(defn- netmask-for [bits]
  (reduce (fn [mask idx] (bit-or mask (bit-shift-left 1 (- 31 idx)))) 0 (range 0 bits)))

(defn- ip-string->long
  "Convert a dotted quad into the corresponding long value"
  [ipstr]
  (address-from-vector (map #(Integer/parseInt %1) (clojure.string/split ipstr #"\."))))

(defn- parse-cidr [str]
  (let [[addr bits] (clojure.string/split str #"/")]
    (when-not (and addr bits)
      (throw (Exception. "Invalid address. You need to supply the address in CIDR notation")))
    [(ip-string->long addr) (Integer/parseInt bits)]))

(defn- low [subnet]
  (:netid subnet))

(defn- high [subnet]
  (:broadcast subnet))

(defn string->subnet-info
  "Reads a string that holds a subnet in CIDR and converts it into an info map

  > (string->subnet-info \"10.0.0.1/19\")
  "
  [str]
  (let [[ip bits] (parse-cidr str)
        netmask   (netmask-for bits)
        network   (bit-and ip netmask)
        ;; we can not go the usual network & ~(netmask) as
        ;; we're dealing with two's complement
        broadcast (bit-or network (bit-xor netmask 0xffffffff))]
    { :address ip
      :netmask netmask
      :netid network
      :broadcast broadcast
      :first network
      :last  broadcast }))

(defn includes?
  "Checks if a given address is included in the given subnet

   > (includes? (string->subnet-info \"192.168.1.0/24\") \"192.168.1.1\")); => true
   > (includes? (string->subnet-info \"192.168.1.0/24\") \"10.168.1.1\")) ; => false
  "
  [subnet address]
  (let [mask 0x0FFFFFFFF
        address' (bit-and (ip-string->long address) mask)
        low'     (bit-and (low subnet) mask)
        high'    (bit-and (high subnet) mask)]
    (and (>= address' low') (<= address' high'))))

(defn member-of-subnet?
  "Checks if a given address is in a given subnet.
   The address and the subnet are expected to be strings in dotted quad format.
   The subnet is expected to use CIDR notation.

   > (member-of-subnet? \"192.168.1.1\" \"192.168.1.0/24\") ; => true
  "
  [addr net]
  (let [subnet (string->subnet-info net)]
    (includes? subnet addr)))
