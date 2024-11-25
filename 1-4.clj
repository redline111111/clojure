(ns lab1_4
  (:require [clojure.string :as str-helper]))

(defn extend-with-char [characters base-str]
  (->> characters
       (filter #(not (str-helper/ends-with? base-str %)))
       (map #(str base-str %))))

(defn merge-combinations [characters string-list]
  (->> string-list
       (map #(extend-with-char characters %))
       (apply concat)))

(defn generate-combinations [chars n]
  (cond
    (<= n 0) '()
    (= (count chars) 0) '()
    (and (= (count chars) 1) (> n 1)) '()
    :else (nth (iterate #(merge-combinations chars %1) chars) (dec n))))

(defn -main []
  (println (generate-combinations '() 3))
  (println (generate-combinations '("a") 1))
  (println (generate-combinations '("a") 3))
  (println (generate-combinations '("a" "b" "c") -3))
  (println (generate-combinations '("a" "b" "c") 2))
  (println (generate-combinations '("a" "b" "c") 3)))

(-main)