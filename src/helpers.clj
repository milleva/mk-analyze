(ns helpers)

(defn ->4digits [n]
  (let [len (count (str n))
        remaining-len (- 4 len)
        zero-seq (apply str (take remaining-len (repeat "0")))]
    (str zero-seq n)))