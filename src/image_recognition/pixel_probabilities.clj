(ns image-recognition.pixel-probabilities
  (:require [image-recognition.image-recognition :refer [get-test-img-color-grid]]))[]

(defn get2d [m x y]
  (-> m (get x) (get y)))

(def classes [1 2 3 4 5 6 7 8 9 10 11 12])

;strucure where all pixels' color info are stored i.e. color counts & color probabilities
(defn new-atom-grid [n]
  (mapv (fn [_]
         (mapv (fn [_] (atom {}))
              (range n))) (range n)))


(defn- get-test-img-count [class])
(defn- inc-test-image-count [class])


(def n 5)
(def test-grid (new-atom-grid 5))

(defn- get-color-count-atom-for-pixel [{:keys [x y]}]
  (let [color-count-atom-grid (get2d test-grid x y)
        color-count-atom (get2d color-count-atom-grid x y)]
    color-count-atom))

(defn add-pixel-color-to-data [class color pixel]
  (let [pixel-color-counts (get-color-count-atom-for-pixel pixel)]
    (prn "shit 2")
    (prn "shit2" pixel-color-counts)
    (if (contains? pixel-color-counts color)
      (swap! pixel-color-counts #(update % color inc)); todo to mutative because atoms
      (swap! pixel-color-counts #(assoc % color 1)))))

;n = pixel grid width (= height)
(defn add-pixels-colors-to-data [class color-grid n]
  (inc-test-image-count class)
  (prn "shit 1")
  (for [x (range n)
        y (range n)
        :let [color (get2d color-grid x y)]]
    (add-pixel-color-to-data class color {:x x :y y})))

(defn read-test-image [class]
  (let [color-grid (get-test-img-color-grid "white_shit" n)]
    (prn "n" n)
    (prn "(count color-grid)" (count color-grid))
    (add-pixels-colors-to-data class color-grid (count color-grid))
    (prn "result")
    (prn @(get2d test-grid 1 1))))







(defn- create-probabilities-for-pixel [color-counts-grid {:keys [x y]} test-img-count]
  (let [color-counts (get2d color-counts-grid x y)]
    (->> (seq color-counts)
         (map
          (fn [[color count]]
            [color (/ count test-img-count)]))
         (into {}))))

(defn generate-probabilites-from-color-counts [class color-counts-grid n]
  (let [test-img-count (get-test-img-count class)
        color-probabilities-grid (map (fn [x]
                                        (map (fn [y]
                                               (create-probabilities-for-pixel color-counts-grid {:x x :y y} test-img-count)
                                               (range n)))) (range n))]
    color-probabilities-grid))

(defn- get-color-probabilities-for-class [class])

(defn probability-that-pixel-is-of-class [class-color-probabilities-grid color {:keys [x y] :as pixel}]
  (let [probability (-> (get2d class-color-probabilities-grid x y)
                        (get color))]
    (or probability 0.01)))

(def class-instance-count 12) ;TODO placements hard coded

(defn get-probability-that-image-is-of-class [class color-grid n]
  (let [p (atom (/ 1 class-instance-count))
        class-color-probabilities-grid (get-color-probabilities-for-class class)]
    (for [x (range n)
          y (range n)
          :let [color (-> color-grid (get x) (get y))
                p-is-of-class (probability-that-pixel-is-of-class class-color-probabilities-grid color {:x x :y y})]]
      (swap! p * p-is-of-class))
    p))

(defn classify-image [classes color-grid n]
  (->> classes
         (sort-by #(get-probability-that-image-is-of-class % color-grid n))
         last))