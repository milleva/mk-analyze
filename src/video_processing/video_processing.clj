(ns video-processing.video-processing
  (:require [clojure.java.io :as io]
            [helpers :refer [->4digits]]
            [kawa.core :refer [ffmpeg!]]))

;ffmpeg -i MKPLACEMENTS_CUT.mov -vf fps=1/3 %04d.png

(defn clip-video [path]
  (ffmpeg! :i path :vf "fps=1/3" "resources/images/clipped/%04d.png"))

(defn clip-test-video []
  (prn "CLIBBING")
  (let [shit (ffmpeg! :i "media/MKPLACEMENTS_CUT.mov" :vf "fps=1/3" "resources/images/%04d.png")]
    (prn "shit " shit)))

(defn safe-delete [name]
  (try
      (do
        (io/delete-file name)
        true)
      (catch Exception _e false)))

(defn ->clipped-image-path [idx]
  (str "resources/images/" (->4digits idx) ".png"))

(defn delete-clipped-images []
  (loop [image-idx 1]
    (let [is-deleted (safe-delete (str "resources/images/" (->clipped-image-path image-idx) ".png"))]
      (when is-deleted
        (recur (inc image-idx))))))
