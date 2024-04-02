(ns image-recognition.naive-bayes)

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850 => 250
;placement location height within pixels 860, 1030 => 170

;Create grid with every 10th pixel. 425 pixels total.
;For each pixel list occurrences of distinct rgb colors.

;first implement only for classes :first & :second
;what is the probability that given only pixel 1, image is :first (and that image is :second)

;TODO
;next implement only for classes :first & :second
;what is the probability that given first two pixels 1 & 2, image is :first (and that image is :second)

;TODO
;next implement for classes :first & :second & :third
;what is the probability that given first two pixels 1 & 2, image is :first (and that image is :second/:third)

;TODO
;next implement for all classes :first, :second, :third ...
;what is the probability that given all pixels 1-425, image is :first (and that image is :second/:third)
;=> which class has highest probability

(def ^:private classes
  [:first :second :third :fourth :fifth :sixth :seventh :eigth :nineth :tenth :eleventh :twelfth])

(defn- distincts->total [distinct-occurrences]
  (reduce + (map second distinct-occurrences)))

(defn- ->class-probabilities [distinct-counts total-count]
  (map (fn [[color count]] [color (/ count total-count)]) distinct-counts))

(defn- get-probability [color probabilities]
  (let [color-p (-> (filter (fn [[c _]] (= c color)) probabilities)
                    first)]
    (if color-p
      (second color-p)
      0.0000001)))

(defn- p-is-first-for-pixel-1 [color first-probabilities second-probabilities]
  (let [p-first (get-probability color first-probabilities)
        p-second (get-probability color second-probabilities)

        logR (- (Math/log p-first) (Math/log p-second))
        R (Math/exp logR)]
    (/ R (+ R 1))))

; -- training data --
(def ^:private distinct-occurrences-first
  {:px1 [["1;2;2" 4]
         ["5;3;2" 5]]
   :px2 [["0;1;2" 4]
         ["3;2;1" 5]]})

(def ^:private distinct-occurrences-second
  {:px1 [["10;0;255" 2]
         ["15;3;2" 7]]
   :px2 [["5;1;2" 6]
         ["3;5;1" 3]]})
; --------------------

(defn bayes-1 []
  (let [px-1-distinct-occurrences-first (:px1 distinct-occurrences-first)
        px-1-distinct-occurrences-second (:px1 distinct-occurrences-second)

        total-occurrences-first (distincts->total px-1-distinct-occurrences-first)
        total-occurrences-second (distincts->total px-1-distinct-occurrences-second)

        first-probabilities-px1 (->class-probabilities px-1-distinct-occurrences-first total-occurrences-first)
        second-probabilities-px1 (->class-probabilities px-1-distinct-occurrences-second total-occurrences-second)

        probability-img-is-first-for-pixel-1 (p-is-first-for-pixel-1 "0;0;0" first-probabilities-px1 second-probabilities-px1)]
    (prn probability-img-is-first-for-pixel-1)
    probability-img-is-first-for-pixel-1))