(ns image-recognition.training-data-collecting
  (:require [clojure.string :as str]
            [file-io :refer [append-res clear-res-txt list-filenames-in-dir
                             read-res]]
            [helpers :refer [divisible? seek]]
            [image-recognition.image-tools :as ir]
            [mikera.image.core :refer [load-image-resource]]))

(def ^:private classes
  #{:first :second :third :fourth :fifth :sixth :seventh :eigth :nineth :tenth :eleventh :twelfth})

(defn- get-existing-content [res-path]
  (let [content (read-res res-path)]
    (str/split content #"\n")))

(defn placement->img-dir-path [class-name]
  (str "images/placement_training_data/" (name class-name) "/"))

(defn placement->text-file-path [class-name]
  (str "text/placement_training_data/" (name class-name) ".txt"))

(defn- add-text-lines [txt-res-path lines]
  (doseq [rgb-str lines]
      (append-res txt-res-path (str rgb-str "\n"))))

(defn- gen-training-data-for-image [class-name img-file]
  (let [txt-res-path (placement->text-file-path class-name)

        ; loop px locations
        x-pxs (filter #(divisible? % 10) (range 1600 1851))
        y-pxs (filter #(divisible? % 10) (range 860 1031))

        existing-rgb-counts (get-existing-content txt-res-path)

        new-rgbs (flatten
                  (map (fn [x]
                         (map (fn [y]
                                (ir/img-rgb-components-at img-file x y))
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

        existing-non-matching-rgb-counts (filter (fn [existing]
                                                   (let [matcher (-> existing
                                                                     (str/split #"p")
                                                                     last)
                                                         matching-new (seek #(str/ends-with? % matcher) new-rgb-counts)]
                                                     (not matching-new)))
                                                 existing-rgb-counts)
        rgb-counts (concat existing-non-matching-rgb-counts new-rgb-counts)]
    (clear-res-txt txt-res-path)
    (add-text-lines txt-res-path rgb-counts)))

(defn- clear-lines-with-single-occurrence [class-name]
  (let [txt-res-path (placement->text-file-path class-name)
        existing-rgb-counts (get-existing-content txt-res-path)
        multi-occurrence-lines (remove #(str/starts-with? % "1 p") existing-rgb-counts)]
    (clear-res-txt txt-res-path)
    (add-text-lines txt-res-path multi-occurrence-lines)))

(defn generate-training-data-from-images [class-name]
  {:pre (classes class-name)}
  (let [img-dir-path (placement->img-dir-path class-name)
        file-names (list-filenames-in-dir img-dir-path)]

    (doseq [file-name file-names]
      (println "Generating training data for" file-name)
      (gen-training-data-for-image
       class-name
       (load-image-resource (str img-dir-path file-name))))
    (println "\nGenerated training data for class" (name class-name) "using" (count file-names) "images.\nCleaning up.")
    (clear-lines-with-single-occurrence class-name)))