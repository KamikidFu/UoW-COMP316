(ns image-example.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)

(defn new-image
  "Function to create a new image."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
)

(defn read-image
  "Function to read an image from a file."
  [filename]
    (let [file (File. filename)]
      (ImageIO/read file)
    )
)

(defn save-image
  "Function to save an image with a particular extension to a file."
  [image extension filename]
    (let [file (File. filename)]
      (ImageIO/write image extension file)
    )
)

(defn get-width
  "Function to get the width of an image."
  [image]
  (.getWidth image)
)

(defn get-height
  "Function to get the height of an image."
  [image]
  (.getHeight image)
)


(defn get-rgb
  "Function to get the RGB components of a pixel in a vector of length 3."
  [image x y]
    (let [rgb (.getRGB image x y)
          red (bit-shift-right (bit-and rgb 0xFF0000) 16)
          blue (bit-shift-right (bit-and rgb 0xFF00) 8)
          green (bit-and rgb 0xFF)
          ]
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

(defn -main
  "Test the image functions."
  [& args]
  (let [filename-in  "kodim14.png"
        filename-out "kodim14-modified.png"
        test-image   (read-image filename-in)
        image-width  (get-width test-image)
        image-height (get-height test-image)
        cropped-image (new-image (/ image-width 2)
                                   image-height)
        ]
    ; report that an image has been read
    (println "image read is" filename-in
             "with dimensions" image-width "X" image-height)
    ; convert the image to greyscale
    (dotimes [x image-width]
      (dotimes [y image-height]
        (let [rgb  (get-rgb test-image x y)
              grey (int (/ (reduce + rgb) 3.0))]
          (set-grey test-image x y grey)
        )
      )
    )
    ; crop half the image into a copy
    (dotimes [x (get-width cropped-image)]
      (dotimes [y (get-height cropped-image)]
        (set-rgb cropped-image
                  x y
                  (get-rgb test-image x y))
        )
    )
    ; save the modified test image
    (save-image cropped-image "png" filename-out)
    ; report
    (println "image written is" filename-out
             "with dimensions" (get-width cropped-image)
             "X" (get-height cropped-image))
  )
)
