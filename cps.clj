(ns cps
  (:refer-clojure :exclude [append partition]))

(defn append-notramp [lst1 lst2 k]
  (cond
   (empty? lst1) (k lst2)
   :else (recur (rest lst1) lst2 
                 (fn [rst]
                   (k (cons (first lst1) rst))))))

(defn append [lst1 lst2 k]
  (cond
   (empty? lst1) #(k lst2)
   :else (recur (rest lst1) lst2 
                 (fn [rst]
                   #(k (cons (first lst1) rst))))))

(defn append-alt [lst1 lst2 k]
  (if-let [[lst1-head & lst1-tail] lst1]
    (recur lst1-tail lst2 
           (fn [rst]
             #(k (cons lst1-head rst))))
    #(k lst2)))

(defn partition-notramp [coll p? k]
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

(defn partition [coll p? k]
  (loop [coll coll k k]
    (cond
     (empty? coll) #(k () ())
     (p? (first coll)) (recur (rest coll)
                              (fn [p-true p-false]
                                #(k (cons (first coll) p-true)
                                   p-false)))
     :else (recur (rest coll)
                  (fn [p-true p-false]
                    (k p-true
                       (cons (first coll)
                             p-false)))))))

(defn quicksort
  ([coll less? k]
     (letfn [(qs [coll k]
                 (if (empty? coll) #(k ())
                     (let [[pivot & coll1] coll]
                       (partition coll1
                                  (fn [x] (less? x pivot))
                                  (fn [less-than greater-than]
                                    (fn []
                                      (qs greater-than
                                          (fn [sorted-gt]
                                            (fn []
                                              (qs less-than
                                                  (fn [sorted-lt]
                                                    (append sorted-lt (cons pivot sorted-gt) k))))))))))))]
       (trampoline (qs coll k))))
     ([coll less?]
        (quicksort coll less? identity)))
        
     
    