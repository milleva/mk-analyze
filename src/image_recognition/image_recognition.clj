(ns image-recognition.image-recognition
  (:use mikera.image.core)
  (:require [clojure.math :as math]))

(defn- flatv [s]
  (-> s flatten vec))

(defn- rgb-difs [{ r1 :r g1 :g b1 :b} {r2 :r g2 :g b2 :b}]
  {:r (abs (- r2 r1))
   :g (abs (- g2 g1))
   :b (abs (- b2 b1))})

(defn- white-rgb? [{:keys [r g b]}]
  (= 255 r g b))

(defn- avg-rgb-dif [rgb1 rgb2]
  (if
   (or (white-rgb? rgb1)
       (white-rgb? rgb2))
    255
    (let [{:keys [r g b]} (rgb-difs rgb1 rgb2)]
      (-> (+ r g b)
          (/ 3.0)))))


(defn rgb-components
  "Return the RGB components of a colour value, in a 3-element map of long values"
  ([^long rgb]
   {:r (bit-shift-right (bit-and rgb 0x00FF0000) 16)
    :g (bit-shift-right (bit-and rgb 0x0000FF00) 8)
    :b (bit-and rgb 0x000000FF)}))

(defn img-rgb-components-at [img x y]
  (let [rgb-long (.getRGB img x y)]
    (rgb-components rgb-long)))

(defn- img-wh [img]
  [(.getWidth img) (.getHeight img)])

(defn- get-image-rgb-grid [img n]
  (let [[w h] (img-wh img)
        interval #(-> %
                     (/ n)
                     (math/floor))
        ->coords-range #(-> (range 0 % (interval %))
                     rest
                     drop-last)
        x-coords (->coords-range w)
        y-coords (->coords-range h)]
    (mapv (fn [x]
            (mapv (fn [y]
                    (img-rgb-components-at img x y))
                  y-coords)) x-coords)))



(defn grid-rgb-difs [img-1 img-2 n]
  (let [grid-1-flat (flatv (get-image-rgb-grid img-1 n))
        grid-2-flat (flatv (get-image-rgb-grid img-2 n))]
    (->> (range (min (count grid-1-flat) (count grid-2-flat)))
         (map
          (fn [i]
           (avg-rgb-dif  (get grid-1-flat i)  (get grid-2-flat i)))))))

(defn matching-grid-pixel-count [img-1 img-2 n]
  (let [rgb-diffs (grid-rgb-difs img-1 img-2 n)]
    (->> rgb-diffs
         (filter #(< % 8))
         count)))

(def placements-ascending [1 2 3 4 5 6 7 8 9 10 11 12])

(defn- load-placement-images []
  (mapv #(load-image-resource (str "images/placements/" % ".png")) placements-ascending))

(defn load-test-placement [n]
  (load-image-resource (str "images/placements/test/" n ".png")))

(defn print-image-data []
  (prn (str "images/" "1st" ".png"))
  )

(defn recognize-placement [path]
  nil)

(defn recognize-item-1 [path]
  nil)

(defn recognize-item-2 [path]
  nil)

