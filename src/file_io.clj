(ns file-io
  (:require [clojure.java.io :as io]))

(defn list-filenames-in-dir [res-path]
  (let [res (io/resource res-path)
        dir (io/file res)
        files (file-seq dir)]
    (map #(.getName %) (rest files))))

(defn write-res [res-path text]
  (let [file (io/resource res-path)]
    (spit file text :append false)))

(defn append-res [res-path text]
  (let [file (io/resource res-path)]
    (spit file text :append true)))

(defn clear-res-txt [res-path]
  (write-res res-path ""))

(defn read-res [res-path]
  (let [file (io/resource res-path)]
    (slurp file)))