;;;; Nathaniel Potrepka
;;;; Fixed-size hash map implementation with linear probing
;;;; Mutable data patterns are used to satisfy challenge requirements.
;;;; Clojure hash maps remain by far superior.

(ns hasher.core)

(defrecord Hash [n data])

;;; Constructor
(defn make-hash
  "Create hash map of size k."
  [k]
  (->Hash
    (make-array Integer/TYPE 1)
    (make-array clojure.lang.PersistentList k)))

;;; Really basic get/set for number of items
(defn- n-get
  "Get number of items in hash map."
  [h]
  (aget (:n h) 0))

(defn- n-set
  "Set number of items in hash map."
  [h n]
  (aset (:n h) 0 n))

(defn- n-inc
  "Increment number of items in hash map."
  [h]
  (n-set h (inc (n-get h))))

(defn- n-dec
  "Decrement number of items in hash map."
  [h]
  (n-set h (dec (n-get h))))

;;; Get total number of buckets
(defn- size
  "Get size of hash map."
  [h]
  (count (:data h)))

;;; Hash function
(defn- hfunc
  "Hash function for hash maps."
  [key]
  (hash key))

;;; Get load factor
(defn hload
  "Get load factor of hash map."
  [h]
  (/ (float (n-get h)) (size h)))

;;; New index
(defn- get-index
  "Allocate index for key in hash table h."
  [h key]
  (let [data (:data h)
        k (size h)]
    (loop [index (mod (hfunc key) k)
           i     0]
      (if (>= i k) ;no more space
        nil
        (if-not (aget data index) ;found empty bucket
          index
          (recur (mod (inc index) k) (inc i))))))) ;linear probing

;;; Existing index
(defn- find-index
  "Find index of key in hash table h. Returns nil if index is not found."
  [h key]
  (let [data (:data h)
        k (size h)]
    (loop [index (mod (hfunc key) k)
           i     0]
      (if (>= i k) ;cannot find
        nil
        (let [key-value (aget data index)]
          (if-not key-value ;cannot find
            nil
            (if (= (first key-value) key) ;found correct key
              index
              (recur (mod (inc index) k) (inc i))))))))) ;linear probing

;;; Set value
(defn hset
  "Sets value for key in hash map. Returns true on success, false on failure."
  [h key value]
  (let [data (:data h)
        f-index (find-index h key)
        index (if-let [i f-index] i (get-index h key))]
    (if index
      (let [key-value (aset data index (list key value))]
        (when-not f-index (n-inc h))
        true)
      false)))

;;; Get value
(defn hget
  "Gets value for key in hash map. Returns value, or nil if no value is set."
  [h key]
  (let [data (:data h)
        index (find-index h key)]
    (when index
      (second (aget data index)))))

;;; Delete value
(defn hdelete
  "Deletes value for key in hash map. Returns value, or nil if no value is set."
  [h key]
  (let [data (:data h)
        index (find-index h key)]
    (when index
      (let [value (hget h key)]
        (aset data index nil)
        (n-dec h)
        value))))
