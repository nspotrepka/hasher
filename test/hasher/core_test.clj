(ns hasher.core-test
  (:require [clojure.test :refer :all]
            [hasher.core :refer :all]))

(defn hash-n
  [h]
  (aget (:n h) 0))

(defn hash-data
  [h]
  (into [] (:data h)))

(deftest make-hash-test
  (testing "make-hash"
    (let [n 8
          h (make-hash n)]
      (is (= (hash-n h) 0))
      (is (= (hash-data h) (vec (repeat n nil)))))))

(deftest hset-test
  (testing "hset"
    (let [n 12
          h (make-hash n)]
      (is (= (hset h "1" "BOBBY") true))
      (hset h "2" "CHARLIE")
      (hset h "3" "DAVID")
      (hset h "4" "EDDY")
      (hset h "5" "FELIX")
      (hset h "6" "GREGORY")
      (hset h "7" "HAROLD")
      (hset h "8" "IVAN")
      (is (= (hash-n h) 8))
      (hset h "9" "JERRY")
      (hset h "10" "KIM")
      (hset h "11" "LANCE")
      (hset h "12" "MAX")
      (is (= (hset h "1" "BILLY") true))
      (is (= (hset h "THIRTEEN" "NATHANIEL") false))
      (is (= (hash-n h) 12)))))

(deftest hget-test
  (testing "hget"
    (let [n 8
          h (make-hash n)]
      (hset h "LOL" [0 1])
      (hset h "NOOB" [2 3])
      (hset h "420" [4 5])
      (is (= (hget h "LOL") [0 1]))
      (is (= (hget h "NOOB") [2 3]))
      (is (= (hget h "420") [4 5]))
      (is (= (hget h "WTF") nil))
      (hset h "420" [7 8])
      (is (= (hget h "420") [7 8]))
      (is (= (hash-n h) 3)))))

(deftest hdelete-test
  (testing "hdelete"
    (let [n 8
          a {:a 5 :b 3 :c 1}
          b {:a 0 :b 1 :c 2}
          c {:a 9 :b 7 :c 8}
          h (make-hash n)]
      (hset h "WILD_MIND" a)
      (hset h "NO_BACKBONE" b)
      (hset h "NEVER_GETTING_A_JOB" c)
      (is (= (hdelete h "NO_BACKBONE") b))
      (is (= (hdelete h "SENSORY_OVERLOAD") nil))
      (is (= (hash-n h) 2)))))

(deftest hload-test
  (testing "hload"
    (let [n 8
          h (make-hash n)]
      (hset h "ONE" "HAPPY")
      (hset h "TWO" "SAD")
      (hset h "THREE" "GLAD")
      (hset h "FOUR" "DAD")
      (is (= (hload h) 0.5)))))
