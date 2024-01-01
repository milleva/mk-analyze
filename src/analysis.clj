(ns analysis
  (:require [video-processing.video-processing :as vp]
            [image-recognition.image-recognition :as ir]))

(def analyzed-image
  {:time-ms 0
   :placement 1
   :item-1 nil
   :item-2 nil})

(defn- analyze-clipped-image [idx]
  {:time-ms idx
   :placement (ir/recognize-placement (vp/->clipped-image-path idx))
   :item-1 (ir/recognize-item-1 (vp/->clipped-image-path idx))
   :item-2 (ir/recognize-item-2 (vp/->clipped-image-path idx))})

(defn- analyze-clipped-images []
  (loop [idx 1
         res []]))

(defn analyze-video [path]
  ;(vp/clip-video path)
  ;analyze images
  ;delete clipped images / cleanup
  ;return vector of analyzed-images
  )

;analyze video path
;clip-video