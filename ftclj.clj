(ns ftclj
  (:refer-clojure :exclude [map take-while])
  (:use clojure.test))

(defn unfold
  ([p f g seed tail-g]
   (lazy-seq
     (if (p seed)
       (tail-g seed)
       (cons (f seed)
             (unfold p f g (g seed))))))
  ([p f g seed]
   (unfold p f g seed (fn [_] ()))))

(defn map [f xs]
  (unfold empty? #(f (first %)) rest xs))

(defn take-while [pred coll]
  (unfold pred first rest coll))

(defn tails [xs]
  (unfold empty? identity rest xs
          #(list %)))

(deftest test-map []
         (is (= (map first []) []))
         (is (= (map (fn [x] (* x 2)) (range 10))
                '(0 2 4 6 8 10 12 14 16 18))))

(deftest test-tails []
         (letfn [(standard-tails [xs]
                                 (cons xs
                                       (when (seq xs)
                                         (tails (rest xs)))))]
           (is (= (tails []) (standard-tails [])))
           (is (= (tails (range 4)) (standard-tails (range 4))))))

(deftest test-take-while []
         (let [c (range 10)]
           (is (= (take-while (fn [x] (< x 5)) c)
                  (clojure.core/take-while (fn [x] (< x 5)) c)))
           (is (= (take-while (fn [x] (< x 0)) c)
                  (clojure.core/take-while (fn [x] (< x 0)) c)))))

(run-tests)
