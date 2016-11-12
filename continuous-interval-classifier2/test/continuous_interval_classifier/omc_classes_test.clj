(ns continuous-interval-classifier.omc-classes-test
  (:require [clojure.test :as t]
            [continuous-interval-classifier.core :as cic])
  (:use clj-time.core clj-time.coerce)
  (:use continuous-interval-classifier.omc-classes :reload))



(t/testing "beginning-of-day"
  (let [now (date-time 2013 10 10 12 24)
        eod (end-of-day now)]
    (t/is (= 2013 (year eod)))
    (t/is (= 10   (month eod)))
    (t/is (= 11   (day eod)))
    (t/is (= 0   (hour eod)))
    (t/is (= 0   (minute eod)))
    (t/is (= 0   (sec eod)))))

(t/testing "intervals for omc-night"
  (let [next-interval (partial cic/next-interval-relative-to p-night)]

    (t/testing "reference before 23 o'clock"
      (let [iv (next-interval (date-time 2013 10 10 22 15))]
        (t/is (= (date-time 2013 10 10 23 0) (first iv)) "starts at 23 o'clock that day")
        (t/is (= (date-time 2013 10 11 6 0) (second iv)) "stops at 6 o'clock next day")))

    (t/testing "reference after 23 o'clock and before 6 o'clock next day"
      (let [iv (next-interval (date-time 2013 10 10 23 15))]
        (t/is (= (date-time 2013 10 10 23 15) (first iv)) "starts at value that day")
        (t/is (= (date-time 2013 10 11 6 0) (second iv)) "stops at 6 o'clock next day")))

    (t/testing "reference before 6 o'clock"
      (let [iv (next-interval (date-time 2013 10 10 5 15))]
        (t/is (= (date-time 2013 10 10 5 15) (first iv)) "starts at value that day")
        (t/is (= (date-time 2013 10 10 6 0) (second iv)) "stops at 6 o'clock that day")))

    (t/testing "reference at 23 o'clock"
      (let [iv (next-interval (date-time 2013 10 10 23 00))]
        (t/is (= (date-time 2013 10 10 23 00) (first iv)) "starts at 23:00")))

    (t/testing "reference at 6 o'clock"
      (let [iv (next-interval (date-time 2013 10 10 6 00))]
        (t/is (= (date-time 2013 10 10 6 00) (first iv)) "starts at 6:00")
        (t/is (= (date-time 2013 10 10 6 00) (second iv)) "starts at 6:00")))))

(t/testing "intervals for default"
  (let [next-interval (partial cic/next-interval-relative-to p-default)]
    (t/testing "for any reference point"
      (let [iv (next-interval (date-time 2013 10 10 10 0))]
        (t/is (= (date-time 2013 10 10 10 0) (first iv)) " starts at reference point")
        (t/is (= (date-time 2013 10 10 10 1) (second iv)) " stops a minute later")))))


(t/testing "intervals for weekend"
  (let [next-interval (partial cic/next-interval-relative-to p-weekend)]
    (t/testing "with reference on weekday"
      (let [iv (next-interval (date-time 2013 7 9 10 0)) ; Tuesday
            next-sunday     (date-time 2013 7 14 0)
            next-sunday-end (date-time 2013 7 15 0 0)]
        (t/is (= next-sunday     (first iv)) "starts at beginning-of next sunday")
        (t/is (= next-sunday-end (second iv)) "stops at end of next sunday")))
    (t/testing "with reference on day saturday"
      (let [iv (next-interval (date-time 2013 7 6 10 0))
            next-sunday     (date-time 2013 7 7 0)
            next-sunday-end (date-time 2013 7 8 0 0)]
        (t/is (= next-sunday     (first iv)) "starts at beginning-of next sunday")
        (t/is (= next-sunday-end (second iv)) "stops at end of next sunday")))
    (t/testing "with reference on sunday"
      (let [iv (next-interval (date-time 2013 7 7 10 0))
            next-sunday-end (date-time 2013 7 8 0 0)]
        (t/is (= (date-time 2013 7 7 10 0)  (first iv)) "starts at refpoint")
        (t/is (= next-sunday-end (second iv)) "stops at end of next sunday")))))

(t/testing "intervals for laborday"
  (let [next-interval (partial cic/next-interval-relative-to p-labor-day)]
    (t/testing "with reference on weekday"
      (let [iv (next-interval (date-time 2013 7 9 3))]
        (t/is (= (date-time 2013 7 9 3) (first iv)) "starts at refpoint")
        (t/is (= (date-time 2013 7 10 0) (second iv)) "stops at 0 that day")))
    (t/testing "with reference on day saturday"
      (let [iv (next-interval (date-time 2013 7 13 3))]
        (t/is (= (date-time 2013 7 13 3) (first iv)) "starts at refpoint")
        (t/is (= (date-time 2013 7 14 0) (second iv)) "stops at 0 that day")))
    (t/testing "with reference on sunday"
      (let [iv (next-interval (date-time 2013 7 14 3))]
        (t/is (= (date-time 2013 7 15 0) (first iv)) "starts at next monday")
        (t/is (= (date-time 2013 7 16 0) (second iv)) "stops at end of next monday")))))

(t/testing "intervals for special-holiday"
  (let [next-interval (partial cic/next-interval-relative-to p-special-holiday)]
    (t/testing "with reference on 1st of may"
      (let [iv (next-interval (date-time 2013 5 1 2 0))]
        (t/is (= (date-time 2013 5 1 2) (first iv)) "starts at 0 that day")
        (t/is (= (date-time 2013 5 2 0) (second iv)) "stops at end of 1st of may")))
    (t/testing "with reference before 1st of may"
      (let [iv (next-interval (date-time 2013 4 20 2 0))]
        (t/is (= (date-time 2013 5 1 0) (first iv)) "starts at next 1st of may")
        (t/is (= (date-time 2013 5 2 0) (second iv)) "stops at end of 1st of may")))
    (t/testing "with reference after 1st of may"
      (let [iv (next-interval (date-time 2013 5 2 2 0))]
        (t/is (= (date-time 2014 5 1 0) (first iv)) "starts at next 1st of may")
        (t/is (= (date-time 2014 5 2 0) (second iv)) "stops at end of 1st of may")))))


(defn collect-result [res]
  (reduce
   (fn [m item]
     (let [duration (in-minutes (interval (minus (first item) (minutes 1)) (second item)))
           cls (:label (last item))
           val (get cls m 0)]
       (assoc m cls (+ val duration))))
   {}
   res))

(defn classify-and-collect [start stop]
  (collect-result (cic/classify-interval [start stop] [p-default p-night p-weekend p-labor-day p-special-holiday])))

(t/testing "complex cases"
  (t/testing "laborday night and weekend"
    (let [start (date-time 2013 7 6 0 0)
          end   (date-time 2013 7 8 0 0)]
      (t/is (= {:omc-weekend 1440, :omc-laborday 1020, :omc-night 60}
               (classify-and-collect start end))))))
