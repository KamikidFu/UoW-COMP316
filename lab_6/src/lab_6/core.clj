(ns lab-6.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)


(defn new-image [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
)

(defn read-image [filename]
  (let [file (File. filename)]
    (ImageIO/read file)
  )
)

(defn save-image [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)
  )
)

(defn get-width [image]
  (.getWidth image)
)

(defn get-height [image]
  (.getHeight image)
)

(defn get-rgb [image x y]
  (let [rgb (.getRGB image x y)
        red (bit-shift-right (bit-and rgb 0xFF0000) 16)
        blue (bit-shift-right (bit-and rgb 0xFF00) 8)
        green (bit-and rgb 0xFF)]
    (vec (list red green blue))
  )
)

(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
     (let [rgb (+ (bit-shift-left red 16)
                  (bit-shift-left blue 8)
                  (bit-shift-left green 0) ) ]
       (.setRGB image x y rgb)
   )
)

(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
   (set-rgb image x y [grey grey grey])
)

(defn q1 [filename]
  (let [processing-image (read-image filename)]
    (save-image processing-image "png" "q1-modified.png")
  )
)

(defn convert-to-intensity [filename r-weight g-weight b-weight]
  (let [image (read-image filename)]
    (when (= 1 (+ r-weight g-weight b-weight))
        (dotimes [x (get-width image)]
          (dotimes [y (get-height image)]
            (let [rgb (get-rgb image x y)
                  r (first rgb)
                  g (second rgb)
                  b (last rgb)
                  grey (int (+ (* r r-weight) (* g g-weight) (* b b-weight)))]
                (set-grey image x y grey)
              )
            )
          )
        (save-image image "png" "q3.png")
      )
    )
)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (let [image (read-image "kodim14.png")
        r-weight 0.2125
        g-weight 0.7154
        b-weight 0.072]
    (dotimes [x (get-width image)]
          (dotimes [y (get-height image)]
            (let [rgb (get-rgb image x y)
                  r (first rgb)
                  g (second rgb)
                  b (last rgb)
                  grey (int (+ (* r r-weight) (* g g-weight) (* b b-weight)))]
                (set-grey image x y grey)
              )
            )
      )
      (save-image image "png" "q3.png")
  )
)
