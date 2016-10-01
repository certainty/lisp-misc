(ns policyj.policies.dns.rbl
  (:use [clojure.string :only (split join)])
  (:import java.net.InetAddress java.net.Inet4Address java.net.Inet6Address))

(defn- dns-resolvable?
  "Tries to resolve the domain passed to the function and returns true if it
   can resolve it and false otherwise. This function is used internally by rbl-check.

   > (dns-resolvable? \"1.0.0.127.rbl.test.org\") ;=> true
  "
  [domain]
  (try
    (not (empty? (InetAddress/getAllByName domain)))
    (catch Exception exn false)))

(defn- reverse-dotted-quad
  "Reverses the IPv4-Address that has to be a string in dotted quad format

   > (reverse-dotted-quad \"127.0.0.1\") ;=> \"1.0.0.127\"
  "
  [ip]
  (join "." (reverse (split ip #"\."))))

(defn- build-rbl-domain
  "Constructs a domain that can be used to do a rbl lookup.

   > (build-rbl-domain 127.0.0.1 rbl.test.org) ; => \"1.0.0.127.rbl.test.org\"
  "
  [ip service]
  (join "." (list (reverse-dotted-quad ip) service)))

(defn rbl-check
  "Performs an rbl check of the ip on the given services.
   It returns the first service on which it resolved or nil if none could resolve.
   If a service is returned it means that the ip is listed by that service.

   > (rbl-check \"127.0.0.1\" [\"rbl.example.org\", \"blacklist.example.org\"]) ;=> service or nil
  "
  [ip services]
  (first (filter #(dns-resolvable? (build-rbl-domain ip %)) services)))
