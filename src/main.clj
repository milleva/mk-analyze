(ns main
  (:use mikera.image.core)
  (:require [clojure.string :as str]
            [image-recognition.image-recognition :refer [compare-classes]]
            [image-recognition.training-data-collecting :refer [fetch-and-parse-training-data
                                                                generate-training-data-from-images
                                                                placement-classes]]
            [video-processing.video-processing :as vp]))

(def ^:private DEFAULT-FPS 1)

(defn clip [_]
  (prn "clip")
  (vp/clip-video "media/first_recording.mkv" DEFAULT-FPS))

(defn clean [_]
  (vp/delete-clipped-images))

(defn clip-new [_]
  (clean _)
  (clip _))

(def type->operation
  {:clip clip
   :clean clean
   :clip-new clip-new})

(defn run [opts]
  (let [{type :type} opts
        operation (or
                   (type type->operation)
                   clip-new)]
    (operation opts)))

;image w, h = [1920, 1080]
;placement location width within pixels 1600, 1850
;placement location height within pixels 860, 1030

(defn test-train [{class :class}]
  (generate-training-data-from-images class))

(defn- uname [str]
  (-> str
      name
      str/upper-case))

(defn ->res-output [p-is-a test-image-name class-a class-b]
  (str "p image '" test-image-name "' is " (uname class-a) " over " (uname class-b) ": " p-is-a))

(defn compare-classes-with-result-ouput [test-image-name class-a class-b]
  (let [p-is-a (compare-classes test-image-name class-a class-b)]
    (println (->res-output p-is-a test-image-name class-a class-b))))

(defn compare-classes-both-ways [test-image-name class-a class-b]
  (let [p-is-a (compare-classes test-image-name class-a class-b)
        p-is-b (compare-classes test-image-name class-b class-a)]

    (prn)
    (println (->res-output p-is-a test-image-name class-a class-b))
    (println (->res-output p-is-b test-image-name class-b class-a))))

(def all-placements (vec placement-classes))
(def unsupported-placements #{:seventh :nineth})
(def supported-placements (remove #(unsupported-placements %) all-placements))

(defn compare-class-with-all-other-classes [test-img-name class-a]
  {:pre (placement-classes class-a)}
  (let [other-classses (remove #(= % class-a) supported-placements)]
    (doseq [class-b other-classses]
      (compare-classes-with-result-ouput test-img-name class-a class-b))))

(defn test-compare [_]
  (let [test-image-name "4a"
        class-a :fourth
        class-b :first]
    (prn)
    (compare-classes-with-result-ouput test-image-name class-a class-b)
    (prn)))

(defn test-compare-all [_]
  (let [test-image-name "4a"
        class-a :first]
    (prn)
    (compare-class-with-all-other-classes test-image-name class-a)
    (prn)))
