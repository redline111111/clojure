(ns sequence-generator.core
  (:gen-class))

(defn has-consecutive-duplicates? [s]
  (if (empty? s)
    false
    (loop [i 0]
      (if (>= (inc i) (count s))
        false
        (if (= (nth s i) (nth s (inc i)))
          true
          (recur (inc i)))))))

(defn generate-sequences [chars n]
  (letfn [(helper [current length]
            (if (= length n)
              (if (not (has-consecutive-duplicates? current))
                [current]
                [])
              (apply concat
                     (for [c chars]
                       (helper (str current c) (inc length))))))]
    (helper "" 0)))

(defn -main []
  (println "Enter a set of characters separated by spaces (for example: a b c):")
  (let [chars (clojure.string/split (read-line) #"\s+")
        _ (println "Enter the number n:")
        n (Integer/parseInt (read-line))]
    (let [sequences (generate-sequences chars n)]
      (println "Result:")
      (doseq [seq sequences]
        (println seq)))))

(-main)