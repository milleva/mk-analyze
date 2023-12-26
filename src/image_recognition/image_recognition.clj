(ns image-recognition.image-recognition
  (:use mikera.image.core)
  (:require [clojure.java.io :as io]
            [clojure.math :as math]))

(defn- flatv [s]
  (-> s flatten vec))

(defn- rgb-difs [{ r1 :r g1 :g b1 :b} {r2 :r g2 :g b2 :b}]
  {:r (abs (- r2 r1))
   :g (abs (- g2 g1))
   :b (abs (- b2 b1))})

(defn- avg-rgb-dif [rgb1 rgb2]
  (let [{:keys [r g b]} (rgb-difs rgb1 rgb2)]
    (-> (+ r g b)
        (/ 3.0))))


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
  (let [grid-1-flat (vec (flatten (get-image-rgb-grid img-1 n)))
        grid-2-flat (vec (flatten (get-image-rgb-grid img-2 n)))]

    (->> (range (min (count grid-1-flat) (count grid-2-flat)))
         (map (fn [i]
           (avg-rgb-dif  (get grid-1-flat i)  (get grid-2-flat i)))))))

(defn matching-grid-pixel-count [img-1 img-2 n]
  (->> (grid-rgb-difs img-1 img-2 n)
         (filter #(< % 10))
         count))

(def placements-ascending ["1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th" "11th" "12th"])

(defn- load-placement-images []
  (mapv #(load-image-resource (str "images/placements/" % ".png")) placements-ascending))

(defn print-image-data []
  (prn (str "images/" "1st" ".png"))
  (let [placement-imgs (load-placement-images)
        [first second third fourth fifth sixth seventh eighth nineth tenth eleventh twleveth] placement-imgs
        third_2 (load-image-resource "images/placements/3rd_2.png")
        nineth_2 (load-image-resource "images/placements/9th_2.png")
        nineth_3 (load-image-resource "images/placements/9th_3.png")

        twleveth_2 (load-image-resource "images/placements/12th_2.png")
        eleventh_2 (load-image-resource "images/placements/11th_2.png")
        eleventh_3 (load-image-resource "images/placements/11th_3.png")

        n 20]

(prn "12 & 12_2" (matching-grid-pixel-count twleveth twleveth_2 n))
(prn "11 & 11_2" (matching-grid-pixel-count eleventh eleventh_2 n))
(prn "11 & 11_3" (matching-grid-pixel-count eleventh eleventh_3 n))
(prn "11_2 & 11_3" (matching-grid-pixel-count eleventh_2 eleventh_3 n))

    (prn)
(prn "11 & 12" (matching-grid-pixel-count first twleveth n))
(prn "11_2 & 12_2" (matching-grid-pixel-count eleventh_2 twleveth_2 n))
(prn "11 & 12_2" (matching-grid-pixel-count first twleveth_2 n))
(prn "11_2 & 12" (matching-grid-pixel-count eleventh_2 twleveth n))

    (prn)

    (prn "first with")
    (prn "2" (matching-grid-pixel-count first second n))
    (prn "3" (matching-grid-pixel-count first third n))
    (prn "4" (matching-grid-pixel-count first fourth n))
    (prn "5" (matching-grid-pixel-count first fifth n))
    (prn "6" (matching-grid-pixel-count first sixth n))
    (prn "7" (matching-grid-pixel-count first seventh n))
    (prn "8" (matching-grid-pixel-count first eighth n))
    (prn "9" (matching-grid-pixel-count first nineth n))
    (prn "10" (matching-grid-pixel-count first tenth n))
    (prn "11" (matching-grid-pixel-count first eleventh n))
    (prn "12" (matching-grid-pixel-count first twleveth n))

    (prn)

    (prn "second with")
    (prn "3" (matching-grid-pixel-count second third n))
    (prn "4" (matching-grid-pixel-count second fourth n))
    (prn "5" (matching-grid-pixel-count second fifth n))
    (prn "6"  (matching-grid-pixel-count second sixth n))
    (prn "7"  (matching-grid-pixel-count second seventh n))
    (prn "8"  (matching-grid-pixel-count second eighth n))
    (prn "9"  (matching-grid-pixel-count second nineth n))
    (prn "10" (matching-grid-pixel-count second tenth n))
    (prn "11" (matching-grid-pixel-count second eleventh n))
    (prn "12" (matching-grid-pixel-count second twleveth n))

    (prn)

    (prn "3rd with")
    (prn "4"(matching-grid-pixel-count third fourth n))
    (prn "5" (matching-grid-pixel-count third fifth n))
    (prn "6"  (matching-grid-pixel-count third sixth n))
    (prn "7"  (matching-grid-pixel-count third seventh n))
    (prn "8"  (matching-grid-pixel-count third eighth n))
    (prn "9"  (matching-grid-pixel-count third nineth n))
    (prn "10" (matching-grid-pixel-count third tenth n))
    (prn "11" (matching-grid-pixel-count third eleventh n))
    (prn "12" (matching-grid-pixel-count third twleveth n))
    (prn)

    (prn "3rd_2 with")
    (prn "3" (matching-grid-pixel-count third_2 third n))
    (prn "4"(matching-grid-pixel-count third_2 fourth n))
    (prn "5" (matching-grid-pixel-count third_2 fifth n))
    (prn "6"  (matching-grid-pixel-count third_2 sixth n))
    (prn "7"  (matching-grid-pixel-count third_2 seventh n))
    (prn "8"  (matching-grid-pixel-count third_2 eighth n))
    (prn "9"  (matching-grid-pixel-count third_2 nineth n))
    (prn "10" (matching-grid-pixel-count third_2 tenth n))
    (prn "11" (matching-grid-pixel-count third_2 eleventh n))
    (prn "12" (matching-grid-pixel-count third_2 twleveth n))
    (prn)

    (prn "4th with")
    (prn "5" (matching-grid-pixel-count fourth fifth n))
    (prn "6"  (matching-grid-pixel-count fourth sixth n))
    (prn "7"  (matching-grid-pixel-count fourth seventh n))
    (prn "8"  (matching-grid-pixel-count fourth eighth n))
    (prn "9"  (matching-grid-pixel-count fourth nineth n))
    (prn "10" (matching-grid-pixel-count fourth tenth n))
    (prn "11" (matching-grid-pixel-count fourth eleventh n))
    (prn "12" (matching-grid-pixel-count fourth twleveth n))
    (prn)

    (prn "9th with")
    (prn "nineth_2" (matching-grid-pixel-count nineth nineth_2 n))
    (prn "nineth_3" (matching-grid-pixel-count nineth nineth_3 n))
    (prn "2" (matching-grid-pixel-count nineth second n))
    (prn "3" (matching-grid-pixel-count nineth third n))
    (prn "4" (matching-grid-pixel-count nineth fourth n))
    (prn "5" (matching-grid-pixel-count nineth fifth n))
    (prn "6" (matching-grid-pixel-count nineth sixth n))
    (prn "7" (matching-grid-pixel-count nineth seventh n))
    (prn "8" (matching-grid-pixel-count nineth eighth n))
    (prn "9" (matching-grid-pixel-count nineth nineth n))
    (prn "10" (matching-grid-pixel-count nineth tenth n))
    (prn "11" (matching-grid-pixel-count nineth eleventh n))
    (prn "12" (matching-grid-pixel-count nineth twleveth n))
    (prn)

    (prn "9th_2 with")
    (prn "nineth_3" (matching-grid-pixel-count nineth_2 nineth_3 n))
    (prn "2" (matching-grid-pixel-count nineth_2 second n))
    (prn "3" (matching-grid-pixel-count nineth_2 third n))
    (prn "4" (matching-grid-pixel-count nineth_2 fourth n))
    (prn "5" (matching-grid-pixel-count nineth_2 fifth n))
    (prn "6" (matching-grid-pixel-count nineth_2 sixth n))
    (prn "7" (matching-grid-pixel-count nineth_2 seventh n))
    (prn "8" (matching-grid-pixel-count nineth_2 eighth n))
    (prn "9" (matching-grid-pixel-count nineth_2 nineth n))
    (prn "10" (matching-grid-pixel-count nineth_2 tenth n))
    (prn "11" (matching-grid-pixel-count nineth_2 eleventh n))
    (prn "12" (matching-grid-pixel-count nineth_2 twleveth n))
    (prn)

    (prn "9th_3 with")
    (prn "2" (matching-grid-pixel-count nineth_3 second n))
    (prn "3" (matching-grid-pixel-count nineth_3 third n))
    (prn "4" (matching-grid-pixel-count nineth_3 fourth n))
    (prn "5" (matching-grid-pixel-count nineth_3 fifth n))
    (prn "6" (matching-grid-pixel-count nineth_3 sixth n))
    (prn "7" (matching-grid-pixel-count nineth_3 seventh n))
    (prn "8" (matching-grid-pixel-count nineth_3 eighth n))
    (prn "9" (matching-grid-pixel-count nineth_3 nineth n))
    (prn "10" (matching-grid-pixel-count nineth_3 tenth n))
    (prn "11" (matching-grid-pixel-count nineth_3 eleventh n))
    (prn "12" (matching-grid-pixel-count nineth_3 twleveth n))
    (prn)
    ))

