(ns image-recognition.naive-bayes
  (:require [clojure.math :refer [log10 pow]]))

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850 => 250
;placement location height within pixels 860, 1030 => 170

;Create grid with every 10th pixel. 468 pixels total.
;For each pixel list occurrences of distinct rgb colors.

;first implement only for classes :first & :second
;what is the probability that given only pixel 1, image is :first (and that image is :second)

;next implement only for classes :first & :second
;what is the probability that given all pixels, image is :first (and that image is :second)

(defn- distincts->total [distinct-occurrences]
  (reduce + (map second distinct-occurrences)))

(defn- ->class-probabilities [distinct-counts total-count]
  (map (fn [[color count]] [color (/ count total-count)]) distinct-counts))

; TODO replace filter first with "seek" helper
(defn- get-probability [color probabilities]
  (let [color-p (-> (filter (fn [[c _]] (= c color)) probabilities)
                    first)]
    (if color-p
      (second color-p)
      0.0000001)))

(defn- p-is-class-a [colors class-a-probabilities class-b-probabilities]
  (let [logRincrements (map-indexed (fn [i color]
                                      (let [class-a-ps (get class-a-probabilities i)
                                            class-b-ps (get class-b-probabilities i)
                                            p-class-a (get-probability color class-a-ps)
                                            p-class-b (get-probability color class-b-ps)]
                                        (- (log10 p-class-a) (log10 p-class-b))))
                                    colors)
        logR (apply + logRincrements)
        R (pow 10 logR)]
    (if (= ##Inf R)
      1
      (/ R (+ R 1)))))

(defn- summarize-training-data [class-training-data]
  (let [total-occurrences (distincts->total (first class-training-data))
        probabilities (mapv #(->class-probabilities % total-occurrences) class-training-data)]
    probabilities))

(defn inputted-bayes-1 [class-a-training-data class-b-training-data test-data]
  (let [class-a-probabilities (summarize-training-data class-a-training-data)
        class-b-probabilities (summarize-training-data class-b-training-data)]
    (p-is-class-a test-data class-a-probabilities class-b-probabilities)))