(ns image-recognition.gpt-naive-bayes)

;CHATGPT implementation when asked
;"write naive bayes image classifier with clojure"
; Does not compile out of the box nor seem to work very well after small adjustments

(defn count-occurrences [coll]
  (reduce (fn [counts elem] (update counts elem (fnil inc 0))) {} coll))

(defn probabilities [coll]
  (let [total (count coll)
        counts (count-occurrences coll)]
    (reduce (fn [probabilities [k v]]
              (assoc probabilities k (/ v total)))
            {}
            counts)))

(defn separate-by-class [data]
  (reduce (fn [acc [class instance]]
            (update acc class conj instance))
          {}
          data))

(defn mean [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn stdev [numbers]
  (let [avg (mean numbers)
        variance (/ (reduce + (map #(* % %) (map #(- % avg) numbers))) (count numbers))]
    (Math/sqrt variance)))

(defn summarize [dataset]
  (map (fn [instances]
         (map (fn [attribute]
                {:mean (mean (map attribute instances))
                 :stdev (stdev (map attribute instances))})
              (apply map list instances)))
       dataset))

(defn calculate-probabilities [x mean stdev]
  (let [exponent (Math/pow (Math/E) (- (/ (Math/pow (- x mean) 2) (* 2 (Math/pow stdev 2)))))]
    (* (/ 1 (Math/sqrt (* 2 Math/PI) (stdev))) exponent)))

(defn calculate-class-probabilities [summaries input]
  (let [probabilities (map (fn [[class class_summary]]
                             (let [class_prob (apply * (map (fn [[k v]]
                                                              (calculate-probabilities (input k) v :mean :stdev))
                                                            class_summary))]
                               [class class_prob]))
                           summaries)]
    (probabilities)))

(defn predict [summaries input]
  (let [class_probabilities (calculate-class-probabilities summaries input)
        [class _] (apply max-key second class_probabilities)]
    class))

(defn naive-bayes [training-data test-data]
  (let [separated (separate-by-class training-data)
        summaries (summarize (map (partial apply map vector) separated))]
    (map (fn [instance]
           (predict summaries (apply hash-map (map vector instance))))
         test-data)))

;; Example usage
(def training-data
  [[:dog 1 1]
   [:dog 2 1]
   [:dog 3 2]
   [:cat 1 3]
   [:cat 2 3]
   [:cat 3 4]])

(def test-data
  [[1 2]
   [2 2]
   [3 1]])

(naive-bayes training-data test-data)
