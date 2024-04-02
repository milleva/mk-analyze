(ns main
  (:use mikera.image.core)
  (:require [analysis :as analysis]
            [image-recognition.image-recognition :as ir]
            [image-recognition.naive-bayes :refer [bayes-1]]
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


(defn tezt [_]
  (let [path "images/clipped/0378.png"
        img (load-image-resource path)]
    (prn (ir/img-wh img))

    (prn "w" (- 1850 1600))
    (prn "h" (- 1030 860))

    (prn (ir/img-rgb-components-at img 10 10))
    (prn (ir/img-rgb-components-at img 10 20))
    (prn (ir/img-rgb-components-at img 10 30))
    (prn (ir/img-rgb-components-at img 20 10))
    (prn (ir/img-rgb-components-at img 20 20))
    (prn (ir/img-rgb-components-at img 20 30))
    ))

(defn test-bayes-1 [_]
  (bayes-1))

