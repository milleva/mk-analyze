(ns analysis
  (:require [image-recognition.image-recognition :as ir]
            [mikera.image.core :refer [load-image-resource]]
            [video-processing.video-processing :as vp]))

; note there are 22 different items (plus "no item")

(def analyzed-image
  {:frame 0
   :placement 1
   :item-1 nil
   :item-2 nil
   :coins 0
   :lap 1})

(defn safe-load-image [name] ;; TODO helpers?
  (try
      (do
        (load-image-resource name)
        true)
      (catch Exception _e false)))

(defn- analyze-clipped-image [idx]
  (let [path (vp/->clipped-image-resource-path idx)
        image (safe-load-image path)]
    (when image
      {:frame idx
       :placement (ir/recognize-placement image)
       :item-1 (ir/recognize-item-1 image)
       :item-2 (ir/recognize-item-2 image)})))

(defn analyze-clipped-images []
  (loop [idx 1
         res []]
    (let [analysis (analyze-clipped-image idx)]
      (if analysis
        (recur (inc idx) (conj res analysis))
        res))))