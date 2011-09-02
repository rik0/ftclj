(ns ftclj
  (:refer-clojure :exclude [map])
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


(deftest test-map []
         (is (= (map first []) []))
         (is (= (map (fn [x] (* x 2)) (range 10))
                '(0 2 4 6 8 10 12 14 16 18))))

(run-tests)
