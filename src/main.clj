(ns main
  (:require [analysis :as analysis]
            [image-recognition.pixel-probabilities :refer [read-test-image]]
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

(defn tezt [_]
  (read-test-image nil))

