(ns main
  (:use mikera.image.core)
  (:require [analysis :as analysis]
            [clojure.string :as str]
            [file-io :refer [append-res clear-res-txt list-filenames-in-dir
                             read-res write-res]]
            [image-recognition.image-recognition-tools :as ir]
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

(defn- seek [pred coll]
  (->> coll
       (filter pred)
       first))

(defn- divisible? [x n]
  (= (mod x n)
     0))

(defn- get-existing-content [res-path]
  (let [content (read-res res-path)]
    (str/split content #"\n")))

(defn train-first [_]
  ;read image rgb components
  ;write to file
  (let [img-dir-path "images/placement_training_data/first/"
        filenames (list-filenames-in-dir img-dir-path)

        filename-1 (first filenames)
        img-file-1 (load-image-resource (str img-dir-path filename-1))

        txt-res-path "text/placement_training_data/first.txt"

        ; loop px locations
        x-pxs (filter #(divisible? % 10) (range 1600 1851))
        y-pxs (filter #(divisible? % 10) (range 860 1031))

        existing-rgb-counts (get-existing-content txt-res-path)

        new-rgbs (flatten
                  (map (fn [x]
                         (map (fn [y]
                                (ir/img-rgb-components-at img-file-1 x y))
                              y-pxs))
                       x-pxs))

        new-rgb-counts (map-indexed (fn [i {:keys [r g b]}]
                                      (let [default-count 1
                                            matcher (str i " " r ";" g ";" b)
                                            matching-existing (seek #(str/ends-with? % matcher) existing-rgb-counts)
                                            existing-count (if matching-existing
                                                             (-> matching-existing
                                                                 (str/split #" ")
                                                                 first
                                                                 Integer/parseInt)
                                                             0)
                                            count (+ default-count existing-count)]
                                        (str count " p" matcher))) new-rgbs)
        _ (prn "new-rgb-counts" new-rgb-counts)

        existing-non-matching-rgb-counts (filter (fn [existing]
                                                   (let [matcher (-> existing
                                                                     (str/split #"p")
                                                                     last)
                                                         matching-new (seek #(str/ends-with? % matcher) new-rgb-counts)]
                                                     (not matching-new)))
                                                 existing-rgb-counts)
        _ (prn "existing-non-matching-rgb-counts" existing-non-matching-rgb-counts)
        rgb-counts (concat existing-non-matching-rgb-counts new-rgb-counts)]
    (clear-res-txt txt-res-path)
    (doseq [rgb-str rgb-counts]
      (append-res txt-res-path (str rgb-str "\n")))))

(defn test-bayes-1 [_]
  (bayes-1))

(defn test-list [_]
  (let [path "images/placement_training_data/first/"]
    (prn (list-filenames-in-dir path))))

(defn test-spit [_]
  (write-res "text/placement_training_data/first.txt" ""))

(defn test-slurp [_]
  (let [res-path "text/placement_training_data/first.txt"
        content (read-res res-path)]
    (str/split content #"\n")))
