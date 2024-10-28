(ns sequence-generator.core
  (:gen-class))

(defn valid-sequence? [seq]
  (not-any? #(= (nth seq %) (nth seq (inc %)))
            (range (dec (count seq)))))

(defn extend-sequence [chars current-seqs]
  (mapcat (fn [seq]
            (for [c chars
                  :let [new-seq (str seq c)]
                  :when (valid-sequence? new-seq)]
              new-seq))
          current-seqs))

(defn generate-combinations [chars n]
  (if (or (<= n 0) (empty? chars))
    []
    (let [initial-seqs chars]
      (reduce (fn [acc _]
                (extend-sequence chars acc))
              initial-seqs
              (range (dec n))))))

(defn -main []
  (println "Enter a set of characters separated by spaces (for example: a b c):")
  (let [chars (clojure.string/split (read-line) #"\s+")
        _ (println "Enter the number n:")
        n (Integer/parseInt (read-line))
        sequences (generate-combinations chars n)]
    (println "Result:")
    (doseq [seq sequences]
      (println seq))))

(-main)