(ns main
  (:use mikera.image.core)
  (:require [image-recognition.naive-bayes :refer [bayes-1]]
            [image-recognition.training-data-collecting :refer [fetch-and-parse-training-data
                                                                generate-training-data-from-images]]
            [video-processing.video-processing :as vp]))

(def ^:private DEFAULT-FPS 2)

(defn clip [_]
  (prn "clip")
  (vp/clip-video "media/first_recording.mkv" DEFAULT-FPS))

(defn clean [_]
  (vp/delete-clipped-images))

(defn clip-new [_]
  (clean _)
  (clip _))

(def type->operation
  {:clip clip
   :clean clean
   :clip-new clip-new})

(defn run [opts]
  (let [{type :type} opts
        operation (or
                   (type type->operation)
                   clip-new)]
    (operation opts)))

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850
;placement location height within pixels 860, 1030

(defn test-train [_]
  (generate-training-data-from-images :second))

(defn test-bayes-1 [_]
  (bayes-1))

(defn tezt [_]
  (prn (fetch-and-parse-training-data :second)))
