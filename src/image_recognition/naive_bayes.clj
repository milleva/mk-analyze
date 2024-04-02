(ns image-recognition.naive-bayes)

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850 => 250
;placement location height within pixels 860, 1030 => 170

;Create grid with every 10th pixel. 425 pixels total.
;For each pixel list occurrences of distinct rgb colors.

;first implement only for classes :first & :second
;what is the probability that given only pixel 1, image is :first (and that image is :second)

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

(defn bayes-1 []
  (let [distinct-occurrences-first [["1;2;2" 4]
                                    ["5;3;2" 5]]
        distinct-occurrences-second [["10;0;255" 2]
                                     ["15;3;2" 7]]

        total-occurrences-first (distincts->total distinct-occurrences-first)
        total-occurrences-second (distincts->total distinct-occurrences-second)

        first-probabilities (->class-probabilities distinct-occurrences-first total-occurrences-first)
        second-probabilities (->class-probabilities distinct-occurrences-second total-occurrences-second)

        probability-img-is-first-for-pixel-1 (p-is-first-for-pixel-1 "0;0;0" first-probabilities second-probabilities)]
    (prn probability-img-is-first-for-pixel-1)
    probability-img-is-first-for-pixel-1))