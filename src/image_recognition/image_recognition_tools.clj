(ns image-recognition.image-recognition-tools
  (:use mikera.image.core))

(defn rgb-components
  "Return the RGB components of a colour value, in a 3-element map of long values"
  ([^long rgb]
   {:r (bit-shift-right (bit-and rgb 0x00FF0000) 16)
    :g (bit-shift-right (bit-and rgb 0x0000FF00) 8)
    :b (bit-and rgb 0x000000FF)}))

(defn img-rgb-components-at [img x y]
  (let [rgb-long (.getRGB img x y)]
    (rgb-components rgb-long)))

(defn img-wh [img]
  [(.getWidth img) (.getHeight img)])



