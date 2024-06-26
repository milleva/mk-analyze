(ns helpers)

(defn spy
  ([o]
   (prn o)
   o)
  ([msg o]
   (prn msg o)
   o))

(defn seek [pred coll]
  (->> coll
       (filter pred)
       first))

(defn divisible? [x n]
  (= (mod x n)
     0))

(defn ->4digits [n]
  (let [len (count (str n))
        remaining-len (- 4 len)
        zero-seq (apply str (take remaining-len (repeat "0")))]
    (str zero-seq n)))