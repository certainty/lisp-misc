(ns continuous-interval-classifier.omc-classes
  (:use clj-time.core clj-time.coerce continuous-interval-classifier.core :reload))

;; our policies work with DateTimes
(extend-protocol ClassificationDomain
  org.joda.time.DateTime
  (successor [val]
    (plus val (-> 1 minutes))))

(defn beginning-of-day [dt]
  (date-time (year dt) (month dt) (day dt) 0 0))

(defn end-of-day [dt]
  (plus (beginning-of-day dt) (hours 24)))

(defn sunday? [dt]
  (= 7 (day-of-week dt)))

(defn saturday? [dt]
  (= 6 (day-of-week dt)))


(defn fifth-of-may-in [year]
  (date-time year 5 1 0 0))

(defn fifth-of-may? [dt]
  (let [fifth-of-may (fifth-of-may-in (year dt))]
    (within? (interval (beginning-of-day fifth-of-may) (end-of-day fifth-of-may)) dt)))

(defrecord PolicyDefault [label prio]
  Classification
  (priority [this] (:prio this))
  ;; matches always and has interval of 1 minute
  (next-interval-relative-to [_ v]
    [v (plus v (minutes 1))]))

(defrecord PolicyNight [label prio]
  Classification
  (priority [this] (:prio this))
  ;; 22-06
  (next-interval-relative-to [_ v]
    (let [today-23   (date-time (year v) (month v) (day v) 23 0)
          today-6    (date-time (year v) (month v) (day v) 6 0)
          next-day-6 (plus (end-of-day v) (hours 6))]
      (cond
       (or (before? v today-6) (= v today-6)) [v today-6]
       (within? (interval today-23 next-day-6) v) [v next-day-6]
       :else [today-23 next-day-6]))))

(defrecord PolicyWeekend [label prio]
  Classification
  (priority [this] (:prio this))
  ;; sunday 00:00 - 00:00
  (next-interval-relative-to [_ dt]
    (if (sunday? dt)
      [dt (plus (beginning-of-day dt) (days 1))]
      ;; next sunday
      (let [days-till-sunday (- 7 (day-of-week dt))
            next-sunday (plus (beginning-of-day dt) (days days-till-sunday))]
        [next-sunday (end-of-day next-sunday)]))))

(defrecord PolicyLaborday [label prio]
  Classification
  (priority [this] (:prio this))
  (next-interval-relative-to [_ dt]
    (if (sunday? dt)
      (let [next-monday (plus (beginning-of-day dt) (days 1))]
      [next-monday (plus next-monday (hours 24))])
      [dt (plus (beginning-of-day dt) (hours 24))])))

(defrecord PolicySpecialHoliday [label prio]
  Classification
  (priority [this] (:prio this))
  (next-interval-relative-to [_ dt]
    (let [this-5-may (fifth-of-may-in (year dt))]
      (cond
       (fifth-of-may? dt)      [dt (end-of-day dt)]
       (before? dt this-5-may) [(beginning-of-day this-5-may) (end-of-day this-5-may)]
       :else                   [(plus this-5-may (years 1)) (end-of-day (plus this-5-may (years 1)))]))))

(def p-default         (PolicyDefault.        :omc-default  0))
(def p-night           (PolicyNight.          :omc-night    3))
(def p-labor-day       (PolicyLaborday.       :omc-laborday 2 ))
(def p-weekend         (PolicyWeekend.        :omc-weekend  4 ))
(def p-special-holiday (PolicySpecialHoliday. :omc-special-holiday 6))
