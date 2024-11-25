(ns lab2_2ы
  (:require [clojure.test :as t]))

(def step-size 1/10)

(defn compute-trapezoid [func x1 x2]
  (* step-size (/ (+ (func x1) (func x2)) 2)))

(defn lazy-integral-sequence [func]
  (reductions + 0 (map #(compute-trapezoid func % (+ % step-size)) (iterate #(+ % step-size) 0))))

(defn integral-function [func]
  (fn [x] (nth (lazy-integral-sequence func) (/ x step-size))))

(def example-integral (integral-function #(* 3 %)))

(defn -main []
  (time (example-integral 10))
  (time (example-integral 10))
  (time (example-integral 15)))

(-main)

(t/deftest integral-tests
  (t/testing "Testing integral calculations"
    (t/is (= 0 ((integral-function #(* 3 %)) 0)))
    (t/is (= 150 ((integral-function #(* 3 %)) 10)))
    (t/is (= -150 ((integral-function #(* -3 %)) 10)))
    (t/is (= 80 ((integral-function #(+ 3 %)) 10)))))

(t/run-tests 'lab-lazy-integrals)