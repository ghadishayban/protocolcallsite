(ns callsite.core
  (:require [criterium.core :refer :all]))

(defprotocol Stress
  (foo [_])
  (bar [_])
  (baz [_]))

(extend-protocol
  Stress
  String
  (foo [x] (str x "FOO"))
  (bar [x] (str x "BAR"))
  (baz [x] (str x "BAZ"))
  Number
  (foo [x] 0)
  (bar [x] 0)
  (baz [x] 0)
  clojure.lang.PersistentVector
  (foo [x] [])
  (bar [x] [])
  (baz [x] []))

(deftype FastPath [x]
    Stress
    (foo [_] (str x "FAST"))
    (bar [_] (str x "FAST"))
    (baz [_] (str x "FAST")))

(defn worst-case-seq [n]
  (->> (cycle ["STRING"
               100
               []])
       (take n)
       (into [])))

(defn best-case-seq [n obj]
  (into [] (repeat n obj)))

(defn fast-path [n]
  (into [] (repeat n (FastPath. "FastPath"))))

(defn stress-test [xs]
  (doseq [x xs]
    (foo x)
    (bar x)
    (baz x)))

(defn clean-unprintables [res]
  (dissoc res :results :outliers))

(defn log-benchmark [fname results]
  (spit fname (clean-unprintables results)))

(defn run-stress-test [benchname fname xs]
  (println "Running" benchname "...")
  (let [result (benchmark (stress-test xs) nil)]
    (println "Mean: " (:mean result))
    (log-benchmark fname result)))

(defn -main [prefix]
  (println "Clojure version " (clojure-version))
  (run-stress-test "Homogenous collection"
                 (str prefix "-best")
                 (best-case-seq 1000000 42))
  (run-stress-test "Heterogenous collection"
                 (str prefix "-worst")
                 (worst-case-seq 1000000))
  (run-stress-test "Deftype collection"
                 (str prefix "-deftype")
                 (fast-path 1000000)))
