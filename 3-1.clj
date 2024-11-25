(ns lab3_1)

(def num-threads (.availableProcessors (Runtime/getRuntime)))

(defn simulate-heavy-even? [num]
  (Thread/sleep 10)
  (even? num))

(defn partition-collection [threads coll]
  (when (seq coll)
    (let [chunk-size (/ (count coll) threads)]
      (cons (take chunk-size coll)
            (partition-collection (dec threads) (drop chunk-size coll))))))

(defn parallel-filter [predicate collection]
  (->> collection
       (partition-collection num-threads)
       (map #(future (doall (filter predicate %))))
       doall
       (mapcat deref)))

(defn -main []
  (time (doall (parallel-filter simulate-heavy-even? (range 100))))
  (time (doall (parallel-filter simulate-heavy-even? (range 100))))
  (time (doall (filter simulate-heavy-even? (range 100)))))

(-main)