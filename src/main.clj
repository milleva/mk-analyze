(ns main
  (:require [image-recognition.image-recognition :as ir]))



(defn run [_opts]
  (ir/print-image-data))

