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

(defn class-name->img-dir-path [class-name]
  (str "images/placement_training_data/" (name class-name) "/"))

(defn class-name->text-file-path [class-name]
  (str "text/placement_training_data/" (name class-name) ".txt"))

(defn- add-text-lines [txt-res-path lines]
  (doseq [rgb-str lines]
      (append-res txt-res-path (str rgb-str "\n"))))

(defn- gen-training-data-for-image [class-name img-file]
  (let [txt-res-path (class-name->text-file-path class-name)

        existing-rgb-counts (get-existing-content txt-res-path)

        ; loop px locations
        x-pxs (filter #(divisible? % 10) (range 1600 1851))
        y-pxs (filter #(divisible? % 10) (range 860 1031))

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
  (let [txt-res-path (class-name->text-file-path class-name)
        existing-rgb-counts (get-existing-content txt-res-path)
        multi-occurrence-lines (remove #(str/starts-with? % "1 p") existing-rgb-counts)]
    (clear-res-txt txt-res-path)
    (add-text-lines txt-res-path multi-occurrence-lines)))

(defn generate-training-data-from-images [class-name]
  {:pre (classes class-name)}
  (let [img-dir-path (class-name->img-dir-path class-name)
        file-names (list-filenames-in-dir img-dir-path)]

    (doseq [file-name file-names]
      (println "Generating training data for" file-name)
      (gen-training-data-for-image
       class-name
       (load-image-resource (str img-dir-path file-name))))
    (println "\nGenerated training data for class" (name class-name) "using" (count file-names) "images.\nCleaning up.")
    (clear-lines-with-single-occurrence class-name)))

; -----------------------
; --- training data -----

(defn- parse-raw-line [raw]
  (let [[count-raw px-raw rgb] (str/split raw #" ")]
    {:count (Integer/parseInt count-raw)
     :pixel (Integer/parseInt (apply str (rest px-raw)))
     :rgb rgb}))

(defn- parse-raw-occurrences [raw-lines]
  (let [occurrences (reduce
                     (fn [acc curr]
                       (let [{:keys [pixel count rgb]} (parse-raw-line curr)
                             existing-px (seek #(= (:pixel %) pixel) acc)
                             remaining (remove #(= (:pixel %) pixel) acc)]
                         (if existing-px
                           (conj remaining (assoc existing-px :val (conj (:val existing-px) [rgb count])))
                           (conj acc {:pixel pixel :val [[rgb count]]}))))
                     []
                     raw-lines)
        sorted-occurrences (sort-by :pixel occurrences)
        parsed-data (map :val sorted-occurrences)]
    parsed-data))

(defn fetch-and-parse-training-data [class-name]
  {:pre (classes class-name)}
  (let [file-path (class-name->text-file-path class-name)
        raw-occurrences (get-existing-content file-path)]
    (parse-raw-occurrences raw-occurrences)))

; -----------------------
; ----- test data -------

; example test data
(def ^:private test-data
  ["15;3;2" "3;5;1"])

(defn generate-parsed-test-data [file-path]
  (let [img-file (load-image-resource (str file-path))

        x-pxs (filter #(divisible? % 10) (range 1600 1851))
        y-pxs (filter #(divisible? % 10) (range 860 1031))

        test-rgbs (flatten
                  (map (fn [x]
                         (map (fn [y]
                                (ir/img-rgb-components-at img-file x y))
                              y-pxs))
                       x-pxs))

        test-data (map (fn [{:keys [r g b]}] (str r ";" g ";" b)) test-rgbs)]))

