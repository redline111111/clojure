(ns lab-exercise
  (:require [clojure.string :as str-util]))

(defn extend-string
  ([characters base-str]
   (extend-string characters base-str []))
  ([characters base-str acc]
   (cond
     (empty? characters) acc
     :else (let [current (first characters)
                 updated-acc (if (str-util/ends-with? base-str current)
                               acc
                               (conj acc (str base-str current)))]
             (recur (rest characters) base-str updated-acc)))))

(defn combine-strings
  ([characters str-list]
   (combine-strings characters str-list []))
  ([characters str-list acc]
   (if (empty? str-list)
     acc
     (let [new-combos (extend-string characters (first str-list))]
       (recur characters (rest str-list) (concat acc new-combos))))))

(defn generate-combinations
  ([characters size]
   (cond
     (<= size 0) []
     (= size 1) characters
     (empty? characters) []
     :else (generate-combinations characters (dec size) characters)))
  ([characters size acc]
   (if (zero? size)
     acc
     (let [new-acc (combine-strings characters acc)]
       (recur characters (dec size) new-acc)))))

(generate-combinations ["a" "b" "c"] 3)

(defn -main []
  (doseq [result [(generate-combinations [] 3)
                  (generate-combinations ["a"] 1)
                  (generate-combinations ["a"] 3)
                  (generate-combinations ["a" "b" "c"] -3)
                  (generate-combinations ["a" "b" "c"] 2)
                  (generate-combinations ["a" "b" "c"] 3)]]
    (println result)))

(-main)