(ns video-processing.video-processing
  (:require [clojure.java.io :as io]
            [helpers :refer [->4digits]]
            [kawa.core :refer [ffmpeg!]]
            [kawa.manager :as manager]))

;ffmpeg -i MKPLACEMENTS_CUT.mov -vf fps=1/3 %04d.png

(defn loop-wait []
  (prn "looping")
  (prn (manager/ls))
  (Thread/sleep 5000)
  (loop-wait)
  )

(defn clip-video
  ([path fps]
  (ffmpeg! :i path :vf (str "fps=" fps) "resources/images/clipped/%04d.png")
   (loop-wait))
  ([path]
  (clip-video path 2)))

(defn safe-delete [name] ;; TODO helpers
  (try
      (do
        (io/delete-file name)
        true)
      (catch Exception _e false)))

(defn ->clipped-image-path [idx] ;; TODO helpers
  (str "resources/images/clipped/" (->4digits idx) ".png"))

(defn ->clipped-image-resource-path [idx] ;; TODO helpers
  (str "images/clipped/" (->4digits idx) ".png"))

(defn delete-clipped-images []
  (loop [image-idx 1]
    (let [is-deleted (safe-delete (->clipped-image-path image-idx))]
      (when is-deleted
        (recur (inc image-idx))))))

(defn list-files-in-dir [path]
  (let [dir (io/file path)
        files (file-seq dir)]
    (prn (take 10 files))
    files))
