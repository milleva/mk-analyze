(ns video-processing.video-processing
  (:require [kawa.core :refer [ffmpeg!]]
            [clojure.java.io :as io]))

;ffmpeg -i MKPLACEMENTS_CUT.mov -vf fps=1/3 %04d.png

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

(defn- ->4digits [n]
  (let [len (count (str n))
        remaining-len (- 4 len)
        zero-seq (apply str (take remaining-len (repeat "0")))]
    (str zero-seq n)))

(defn delete-clipped-images []
  (loop [image-idx 1]
    (let [is-deleted (safe-delete (str "resources/images/" (->4digits image-idx) ".png"))]
      (when is-deleted
        (recur (inc image-idx))))))
