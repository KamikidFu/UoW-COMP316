(ns lab-7.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)


(defn get-width [image]
  (.getWidth image)
)

(defn get-height [image]
  (.getHeight image)
)

(defn read-image [filename]
  (let [file (File. filename)]
    (ImageIO/read file)
  )
)

(defn get-rgb [image x y]
  (let [rgb (.getRGB image x y)
        red (bit-shift-right (bit-and rgb 0xFF0000) 16)
        blue (bit-shift-right (bit-and rgb 0xFF00) 8)
        green (bit-and rgb 0xFF)]
    (vec (list red green blue))
  )
)

(defn save-image [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)
  )
)

(defn extract-16bin-histo [filename]
  (let [image (read-image filename)]

  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
