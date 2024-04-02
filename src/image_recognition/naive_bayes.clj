(ns image-recognition.naive-bayes
  (:require [clojure.math :refer [log10 pow]]))

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850 => 250
;placement location height within pixels 860, 1030 => 170

;Create grid with every 10th pixel. 425 pixels total.
;For each pixel list occurrences of distinct rgb colors.

;first implement only for classes :first & :second
;what is the probability that given only pixel 1, image is :first (and that image is :second)

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

(defn- p-is-class-1 [colors class-1-probabilities class-2-probabilities]
  (let [logRincrements (map-indexed (fn [i color]
                             (let [class-1-ps (get class-1-probabilities i)
                                   class-2-ps (get class-2-probabilities i)
                                   p-class-1 (get-probability color class-1-ps)
                                   p-class-2 (get-probability color class-2-ps)]
                               (- (log10 p-class-1) (log10 p-class-2))))
                           colors)
        logR (apply + logRincrements)
        R (pow 10 logR)]
    (/ R (+ R 1))))

(defn- summarize-training-data [class-training-data]
  (let [total-occurrences (distincts->total (first class-training-data))
        probabilities (mapv #(->class-probabilities % total-occurrences) class-training-data)]
    probabilities))

(defn inputted-bayes-1 [class-1-training-data class-2-training-data test-data]
  (let [class-1-probabilities (summarize-training-data class-1-training-data)
        class-2-probabilities (summarize-training-data class-2-training-data)]
    (p-is-class-1 test-data class-1-probabilities class-2-probabilities)))

; -- training data --
; assumed same amount of total occurrences for all pixels per class
(def ^:private distinct-occurrences-first
  [[["1;2;2" 4] ;px1
    ["5;3;2" 5]]
   [["0;1;2" 4] ;px2
    ["3;2;1" 5]]])

(def ^:private distinct-occurrences-second
 [[["10;0;255" 2]
   ["15;3;2" 7]]
  [["5;1;2" 6]
   ["3;5;1" 3]]])
; --------------------

(def ^:private test-data
  ["15;3;2" "3;5;1"])

(defn bayes-1 []
  (inputted-bayes-1 distinct-occurrences-first distinct-occurrences-second test-data))