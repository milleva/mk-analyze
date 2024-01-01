(ns main
  (:require [image-recognition.image-recognition :as ir]
            [video-processing.video-processing :as vp]))



(defn run [_opts]
  ;(ir/print-image-data)
  ;(vp/clip-test-video)
  (vp/delete-clipped-images)
  ;(vp/del-file "resources/images/%04d.png")
  )

