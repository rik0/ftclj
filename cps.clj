(ns cps
  (:refer-clojure :exclude [append partition]))

(defn append [lst1 lst2 k]
  (cond
   (empty? lst1) (k lst2)
   :else (append (rest lst1) lst2 
                 (fn [rst]
                   (k (cons (first lst1) rst))))))

(defn partition [coll p? k]
  (loop [coll coll k k]
    (cond
     (empty? coll) (k () ())
     (p? (first coll)) (recur (rest coll)
                              (fn [p-true p-false]
                                (k (cons (first coll) p-true)
                                   p-false)))
     :else (recur (rest coll)
                  (fn [p-true p-false]
                    (k p-true
                       (cons (first coll)
                             p-false)))))))

(defn quicksort
  ([coll less? k]
     (letfn [(qs [coll k]
                (if (empty? coll) (k ())
                    (let [pivot (first coll)
                          coll1 (rest coll)]
                      (partition coll1
                                 (fn [x] (less? x pivot))
                                 (fn [less-than greater-than]
                                   (qs greater-than
                                          (fn [sorted-gt]
                                            (qs less-than
                                                (fn [sorted-lt]
                                                  (append sorted-lt (cons pivot sorted-gt) k))))))))))]
       (qs coll k)))
  ([coll less?]
     (quicksort coll less? identity)))
        
     
    