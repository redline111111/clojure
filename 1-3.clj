(ns custom-functions.core
  (:gen-class))

(defn my-map [f coll]
  (reduce (fn [acc x]
            (cons (f x) acc))
          '()
          (reverse coll)))

(defn my-filter [pred coll]
  (reduce (fn [acc x]
            (if (pred x)
              (cons x acc)
              acc))
          '()
          (reverse coll)))

(defn -main []
  (println "my-map with inc function on [1 2 3]:")
  (println (my-map inc [1 2 3]))
  
  (println "my-map with #(* % 2) function on [1 2 3]:")
  (println (my-map #(* % 2) [1 2 3]))
  
  (println "my-filter with odd? predicate on [1 2 3 4 5]:")
  (println (my-filter odd? [1 2 3 4 5]))
  
  (println "my-filter with #(> % 2) predicate on [1 2 3 4 5]:")
  (println (my-filter #(> % 2) [1 2 3 4 5])))

(-main)