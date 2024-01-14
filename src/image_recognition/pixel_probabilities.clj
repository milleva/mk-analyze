(ns image-recognition.pixel-probabilities)

(defn- get-test-img-count [class])
(defn- inc-test-image-count [class])

(defn- get-color-count-atom-for-pixel [pixel]
  )

(defn add-pixel-color-to-data [class pixel]
  (let [pixel-color-counts (get-color-count-atom-for-pixel pixel)
        {color :color} pixel]
    (if (contains? pixel-color-counts color)
      (update pixel-color-counts color inc); todo to mutative because atoms
      (assoc pixel-color-counts color 1))))

;n = pixel grid width (= height)
(defn add-pixels-colors-to-data [class color-grid n]
  (inc-test-image-count class)
  (for [x (range n)
        y (range n)
        :let [color (-> color-grid
                        (get x)
                        (get y))]]
    (add-pixel-color-to-data class color)))


(defn- create-probabilities-for-pixel [color-counts-grid {:keys [x y] :as pixel} test-img-count]
  (let [color-counts (-> color-counts-grid (get x) (get y))]
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
  (let [probability (-> class-color-probabilities-grid
                        (get x)
                        (get y)
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
  (let [classes-and-probabilities (map (fn [class]
                                         {:class class
                                          :probability (get-probability-that-image-is-of-class class color-grid n)})
                                       classes)
        best-class (->> classes-and-probabilities
                        (sort-by :probability)
                        last
                        :class)]
    best-class))