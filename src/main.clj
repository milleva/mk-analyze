(ns main
  (:use mikera.image.core)
  (:require [analysis :as analysis]
            [clojure.string :as str]
            [file-io :refer [list-filenames-in-dir read-res write-res]]
            [image-recognition.naive-bayes :refer [bayes-1]]
            [image-recognition.training-data-collecting :refer [generate-training-data-from-images]]
            [video-processing.video-processing :as vp]))

(def ^:private DEFAULT-FPS 2)

(defn clip [_]
  (prn "clip")
  (vp/clip-video "media/first_recording.mkv" DEFAULT-FPS))

(defn analyze [_]
  (prn "analysis")
  (prn (analysis/analyze-clipped-images)))

(defn clean [_]
  (vp/delete-clipped-images))

(defn clip-new [_]
  (clean _)
  (clip _))

(def type->operation
  {:clip clip
   :analyze analyze
   :clean clean
   :clip-new clip-new})

(defn run [opts]
  (let [{type :type} opts
        operation (or
                   (type type->operation)
                   analyze)]
    (operation opts)))

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850
;placement location height within pixels 860, 1030

(defn test-train [_]
  (generate-training-data-from-images :second))

(defn test-bayes-1 [_]
  (bayes-1))

(defn test-list [_]
  (let [path "images/placement_training_data/first/"]
    (prn (list-filenames-in-dir path))))

(defn test-spit [_]
  (write-res "text/placement_training_data/first.txt" ""))

(defn test-slurp [_]
  (let [res-path "text/placement_training_data/first.txt"
        content (read-res res-path)]
    (str/split content #"\n")))
