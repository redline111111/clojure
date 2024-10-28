(ns numerical-integration.core)

(defn trapezoidal-integral [f]
  (let [step 0.01 
        cache (atom {})] 
    (fn [x]
      (if-let [cached-result (@cache x)]
        (do
          (println "Extracting the value for x from the cache =" x)
          cached-result)
        (let [n (int (/ x step))
              result (* step
                        (+ (/ (+ (f 0) (f x)) 2) ;; Половина суммы начальной и конечной точек
                           (reduce + (map (fn [k] (f (* k step))) (range 1 n))))) ;; Площадь внутренних точек
              ]
          (swap! cache assoc x result)
          result)))))

(defn test-integration []
  (let [tol 0.01
        tests [{:f (fn [t] t)
                :x 2
                :expected 2.0
                :description "f(t) = t, integral from 0 to 2 = 2.0"}
               {:f (fn [t] (* t t))
                :x 2
                :expected (/ 8.0 3)
                :description "f(t) = t^2, integral from 0 to 2 ≈ 2.6667"}
               {:f (fn [t] 1)
                :x 2
                :expected 2.0
                :description "f(t) = 1, integral from 0 to 2 = 2.0"}
               {:f (fn [t] (Math/exp t))
                :x 1
                :expected (- (Math/exp 1) 1)
                :description "f(t) = e^t, integral from 0 to 1 ≈ 1.71828"}
               {:f (fn [t] (Math/sin t))
                :x Math/PI
                :expected 2.0
                :description "f(t) = sin(t), integral from 0 to π ≈ 2"}
               {:f (fn [t] (Math/cos t))
                :x (/ Math/PI 2)
                :expected 1.0
                :description "f(t) = cos(t), integral from 0 to π/2 = 1"}]]
    (doseq [{:keys [f x expected description]} tests]
      (let [F (trapezoidal-integral f)
            result (F x)
            error (Math/abs (- result expected))]
        (println description)
        (println "Numerical result:" result)
        (println "Expected value:" expected)
        (println "Error rate:" error)
        (if (< error tol)
          (println "The test is passed")
          (println "Test failed"))
        (println "----------------------------------")))))

;; Запуск тестов
(defn -main []
    (test-integration)
    (let [f (fn [t] (* t t)) 
        F (trapezoidal-integral f)]
    (println "The result for x = 2:" (F 2))
    (println "The result for x = 2:" (F 2))
    (println "The result for x = 3:" (F 3))
    (println "The result for x = 3:" (F 3))))
(-main)
