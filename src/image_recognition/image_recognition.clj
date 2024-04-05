(ns image-recognition.image-recognition
  (:require [image-recognition.naive-bayes :refer [inputted-bayes-1]]
            [image-recognition.training-data-collecting :refer [fetch-and-parse-training-data
                                                                generate-parsed-test-data]]))

(defn compare-classes [test-img-name class-a class-b]
  (let [test-data-path (str "images/test/" test-img-name ".png")
        test-data (generate-parsed-test-data test-data-path)
        first-training-data (fetch-and-parse-training-data class-a)
        second-training-data (fetch-and-parse-training-data class-b)]
    (inputted-bayes-1 first-training-data second-training-data test-data)))