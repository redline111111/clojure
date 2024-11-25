(ns lab-task-integrals
  (:require [clojure.test :as t]))

(def step-size 1/100)

(defn trapezoidal-area [func x1 x2]
  (* step-size (/ (+ (func x1) (func x2)) 2)))

(defn recursive-integral [func start end recur-fn]
  (if (>= start end)
    0
    (+ (recur-fn func start (- end step-size) recur-fn)
       (trapezoidal-area func (- end step-size) end))))

(def cached-recursive-integral (memoize recursive-integral))

(defn cached-integral [func]
  (fn [x] (cached-recursive-integral func 0 x cached-recursive-integral)))

(defn standard-integral [func]
  (fn [x] (recursive-integral func 0 x recursive-integral)))

(def integral-mem-example (cached-integral #(* 3 %)))
(def integral-example (standard-integral #(* -3 %)))

(defn -main []
  (time (integral-example 5))
  (time (integral-example 5))
  (time (integral-example 10))
  (time (integral-mem-example 5))
  (time (integral-mem-example 6))
  (time (integral-mem-example 7))
  (time (integral-mem-example 10)))

(-main)

(t/deftest integral-tests
  (t/testing "Integral function tests"
    (t/is (= 0 ((cached-integral #(* 3 %)) -2)))
    (t/is (= 0 ((cached-integral #(* 3 %)) 0)))
    (t/is (= 150 ((cached-integral #(* 3 %)) 10)))
    (t/is (= -150 ((cached-integral #(* -3 %)) 10)))))

(t/run-tests 'lab-task-integrals)