;;==============================================
;;This assignment is for COMP316-18A Assignment 3
;;Computer Vision
;;==============================================
;;Author: Yunhao Fu
;;ID: 1255469
;;*Kindly note that during the time of doing this assignment, I have disscusions with Zhe Wang, Zixu Zhang, Yuyang Du and Jiayi Hu
;;*I would like to share my ideas and concern other ideas from them. I, here by, declare that the construction of this work is by myself individually.
(ns as3.core
  (:gen-class)
  ;Import java library
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)

;;A simple method to reload the project in the repl
(defn reload []
  (use 'as3.core :reload)
  :RELOADED!
)

;;Store all the kirsh-filters in a variable
(def kirsh-filters [
                     [[-1 0 1][-2 0 2][-1 0 1]]
                     [[-2 -1 0][-1 0 1][0 1 2]]
                     [[-1 -2 -1][0 0 0][1 2 1]]
                     [[0 -1 -2][1 0 -1][2 1 0]]
                     [[1 0 -1][2 0 -2][1 0 -1]]
                     [[2 1 0][1 0 -1][0 -1 -2]]
                     [[1 2 1][0 0 0][-1 -2 -1]]
                     [[0 1 2][-1 0 1][-2 -1 0]]
                   ])

;;=======================================================================================
;;These code are retrived from image-explame project on moodle.
;;Some of the code got some changes to fit this assignment.
;;=======================================================================================
;;new-image function uses BufferedImage to generate a new image with defined width height
(defn new-image [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
)

;;get-width function uses library class function to get width of image
(defn get-width [image]
  (.getWidth image)
)

;;get-height function does a similar thing as get-width
(defn get-height [image]
  (.getHeight image)
)

;;read-image uses image io to read image file
(defn read-image [filename]
  (let [file (File. filename)]
    (ImageIO/read file)
  )
)

;;save-image is for research use
(defn save-image [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)
  )
)

;;get-rgb use image and x y position
(defn get-rgb [image x y]
  (if (and (and (and (> x 0) (> y 0)) (< x (get-width image))) (< y (get-height image)))
    (let [rgb (.getRGB image x y)
          red (bit-shift-right (bit-and rgb 0xFF0000) 16)
          blue (bit-shift-right (bit-and rgb 0xFF00) 8)
          green (bit-and rgb 0xFF)]
      (vec (list red green blue))
    )
    nil
  )
)

;;get-grey needs to get-rgb first and then convert to grey intensity
(defn get-grey [image x y]
  "Use average method to calculate greyscale"
  (let [rgb (get-rgb image x y)]
    (if (not (nil? rgb))
      (int (/ ( reduce + rgb ) 3.0))
      nil
    )
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

;;===============================================================================
;;This part is mainly for assignment.
;;===============================================================================

;;====================================
;;Part 1
;;====================================
;;two-vec-muladd is going to applied filter to a vector of grey intensity
(defn two-vec-muladd [grey-vec filter-vec]
  (when (= (count grey-vec) (count filter-vec))
    ;;use looping though each pixel point and conduct point operation on that pixel
      (loop [result 0
             processing-vec1 grey-vec
             processing-vec2 filter-vec]
        (if (empty? processing-vec1)
          result
          (recur (if (nil? (first processing-vec1))
                   result
                   (+ result (* (first processing-vec1) (first processing-vec2)))
                 )
                 (rest processing-vec1)
                 (rest processing-vec2)
          )
        )
      )
  )
)


;;get-grey-matrix is to get a vector of pixels intensity value
(defn get-grey-matrix [image x y]
  [(get-grey image (- x 1) (- y 1))
   (get-grey image x (- y 1))
   (get-grey image (+ x 1) (- y 1))
   (get-grey image (- x 1) y)
   (get-grey image x y)
   (get-grey image (+ x 1) y)
   (get-grey image (- x 1) (+ y 1))
   (get-grey image x (+ y 1))
   (get-grey image (+ x 1) (+ y 1))]
)

;;kirsh filter operation on point
(defn kirsh-filter-po [ori-image re-image k-filter x y]
  ;;grey-matrix is a vector of intensity of pixel and 8 other pixels around it
  (let [grey-matrix (get-grey-matrix ori-image x y)
        ;;calculate the filter applied grey value
        greyscale (+ 127 (two-vec-muladd grey-matrix (flatten k-filter)))]
    ;;Clampping grey value
    (cond
      (< greyscale 0) (set-grey re-image x y 0)
      (> greyscale 255) (set-grey re-image x y 255)
      :else (set-grey re-image x y greyscale)
    )
    ;;return image
    re-image
  )
)
;;kirsh-filter is a method of nested looping different pixel
(defn kirsh-filter [image k-filter]
  (let [return-image (new-image (get-width image) (get-height image))]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (kirsh-filter-po image return-image k-filter x y)
      )
    )
    return-image
  )
)
;;kirsh is required method to apply kirsh filter
(defn kirsh [file i]
  (let [image (read-image file)]
    (kirsh-filter image (nth kirsh-filters i))
  )
)

;;====================================
;;Part 2
;;====================================
;;mag-po is a point operation of edge magnitude
(defn mag-po [ori-image x y]
  ;;grey-matrix is a vector of intensity of pixel and 8 other pixels around it
  (let [grey-matrix (get-grey-matrix ori-image x y)]
    ;;loop to get the max value of pixels
    (loop [results []
           processing-vec kirsh-filters]
      (if (empty? processing-vec)
        (cond
          ;;do clampping when it is out of 8 bits
          (< (apply max results) 0) 0
          (> (apply max results) 255) 255
          :else (apply max results)
        )
        (recur
               (conj results (+ 127 (two-vec-muladd grey-matrix (flatten (first processing-vec)))))
               (rest processing-vec)
        )
      )
    )
  )
)
;;dir-po is a point operation of edge direction
(defn dir-po [ori-image x y]
  ;;grey-matrix is a vector of intensity of pixel and 8 other pixels around it
  (let [grey-matrix (get-grey-matrix ori-image x y)]
    ;;loop to get the filter no. of max value
    (loop [results []
           processing-vec kirsh-filters]
      (if (empty? processing-vec)
        ;;get the filter index
        (.indexOf results (apply max results))
        (recur
               (conj results (+ 127 (two-vec-muladd grey-matrix (flatten (first processing-vec)))))
               (rest processing-vec)
        )
      )
    )
  )
)

;;edge-magnitude-hist is a required method
;;this method is going to generate a histogram
(defn edge-magnitude-hist [filename]
  ;;read the image first and then using atom to record any number changes
  (let [
        image (read-image filename)
        v0 (atom 0.0)
        v1 (atom 0.0)
        v2 (atom 0.0)
        v3 (atom 0.0)
        v4 (atom 0.0)
        v5 (atom 0.0)
        v6 (atom 0.0)
        v7 (atom 0.0)]
    ;;nested looping though all pixels
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
       (let [current-counting (int (/ (mag-po image x y) 32))]
         (case current-counting
           0 (swap! v0 inc)
           1 (swap! v1 inc)
           2 (swap! v2 inc)
           3 (swap! v3 inc)
           4 (swap! v4 inc)
           5 (swap! v5 inc)
           6 (swap! v6 inc)
           7 (swap! v7 inc)
         )
       )
      )
    )
    ;;return a hash map of histogram with normalised value
    (let [total-pixels (+ @v0 @v1 @v2 @v3 @v4 @v5 @v6 @v7)]
      {:0-31 (Double. (format "%.2f" (/ @v0 total-pixels)))
       :32-63 (Double. (format "%.2f" (/ @v1 total-pixels)))
       :64-95 (Double. (format "%.2f" (/ @v2 total-pixels)))
       :96-127 (Double. (format "%.2f" (/ @v3 total-pixels)))
       :128-159 (Double. (format "%.2f" (/ @v4 total-pixels)))
       :160-191 (Double. (format "%.2f" (/ @v5 total-pixels)))
       :192-223 (Double. (format "%.2f" (/ @v6 total-pixels)))
       :224-255 (Double. (format "%.2f" (/ @v7 total-pixels)))}
    )
  )
)

;;edge-direction-hist is similar to edge-magnitude-hist
(defn edge-direction-hist [filename]
  (let [image (read-image filename)
        v0 (atom 0.0)
        v1 (atom 0.0)
        v2 (atom 0.0)
        v3 (atom 0.0)
        v4 (atom 0.0)
        v5 (atom 0.0)
        v6 (atom 0.0)
        v7 (atom 0.0)]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
       (let [current-counting (dir-po image x y)]
         (case current-counting
           0 (swap! v0 inc)
           1 (swap! v1 inc)
           2 (swap! v2 inc)
           3 (swap! v3 inc)
           4 (swap! v4 inc)
           5 (swap! v5 inc)
           6 (swap! v6 inc)
           7 (swap! v7 inc)
         )
       )
      )
    )
    ;;normalise the histogram
    (let [total-pixels (+ @v0 @v1 @v2 @v3 @v4 @v5 @v6 @v7)]
      {:0 (Double. (format "%.2f" (/ @v0 total-pixels)))
       :1 (Double. (format "%.2f" (/ @v1 total-pixels)))
       :2 (Double. (format "%.2f" (/ @v2 total-pixels)))
       :3 (Double. (format "%.2f" (/ @v3 total-pixels)))
       :4 (Double. (format "%.2f" (/ @v4 total-pixels)))
       :5 (Double. (format "%.2f" (/ @v5 total-pixels)))
       :6 (Double. (format "%.2f" (/ @v6 total-pixels)))
       :7 (Double. (format "%.2f" (/ @v7 total-pixels)))}
    )
  )
)


;;====================================
;;Part 3
;;====================================

(defn intensity-hist [filename]
  (let [image (read-image filename)
        v0 (atom 0.0)
        v1 (atom 0.0)
        v2 (atom 0.0)
        v3 (atom 0.0)
        v4 (atom 0.0)
        v5 (atom 0.0)
        v6 (atom 0.0)
        v7 (atom 0.0)]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        ;;For intensity, get the grey value first and then put it into histogram
        (let [current-counting (if-not (nil? (get-grey image x y))
                                 (int (/ (get-grey image x y) 32))
                                 -1
                                )]
          (case current-counting
            0 (swap! v0 inc)
            1 (swap! v1 inc)
            2 (swap! v2 inc)
            3 (swap! v3 inc)
            4 (swap! v4 inc)
            5 (swap! v5 inc)
            6 (swap! v6 inc)
            7 (swap! v7 inc)
            -1
          )
        )
      )
    )
    ;;normalise the histogram
    (let [total-pixels (+ @v0 @v1 @v2 @v3 @v4 @v5 @v6 @v7)]
      {:0-31 (Double. (format "%.2f" (/ @v0 total-pixels)))
       :32-63 (Double. (format "%.2f" (/ @v1 total-pixels)))
       :64-95 (Double. (format "%.2f" (/ @v2 total-pixels)))
       :96-127 (Double. (format "%.2f" (/ @v3 total-pixels)))
       :128-159 (Double. (format "%.2f" (/ @v4 total-pixels)))
       :160-191 (Double. (format "%.2f" (/ @v5 total-pixels)))
       :192-223 (Double. (format "%.2f" (/ @v6 total-pixels)))
       :224-255 (Double. (format "%.2f" (/ @v7 total-pixels)))}
    )
  )
)

;;image-descriptor is required method to put three vectors together
(defn image-descriptor [filename]
  (let [mag-value (vals (edge-magnitude-hist filename))
        dir-value (vals (edge-direction-hist filename))
        int-value (vals (intensity-hist filename))]
    (vec (concat mag-value dir-value int-value))
  )
)


;;====================================
;;Part 4
;;====================================
;;image-similarity is to compare two description of images and find its image-similarity
(defn image-similarity [filename1 filename2]
  (let [des1 (image-descriptor filename1)
        des2 (image-descriptor filename2)]
    (/ (apply + (map min des1 des2)) 3.0)
  )
)


;;====================================
;;Part 5
;;====================================
;;Following blur/smooth/laplace methods are quite similar to each other
;;But threshold method has a Q123 method to get the Q1, Q2(median) and Q3 point of whole pixels
;;====================================
;;blur is for linear bluring filter
;;po is for point operation
(defn linear-blur-po [ori-image re-image x y]
  (let [grey-matrix (get-grey-matrix ori-image x y)
        grey  (int (two-vec-muladd grey-matrix [1/9 1/9 1/9 1/9 1/9 1/9 1/9 1/9 1/9]))]
    (set-grey re-image x y grey)
    re-image
  )
)

;;blur is the main method to call applying the filter
(defn blur [filename]
  (let [image (read-image filename)
         return-image (new-image (get-width image)(get-height image))]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (linear-blur-po image return-image x y)
      )
    )
    return-image
  )
)

;;smooth-po is similar to linear-blur-po
(defn smooth-po [ori-image re-image x y]
  (let [grey-matrix (get-grey-matrix ori-image x y)
        grey (int (two-vec-muladd grey-matrix [0.075 0.125 0.075 0.125 0.2 0.125 0.075 0.125 0.075]))]
    (set-grey re-image x y grey)
    re-image
  )
)

;;smooth is similar to blur method
(defn smooth [filename]
  (let [image (read-image filename)
         return-image (new-image (get-width image)(get-height image))]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (smooth-po image return-image x y)
      )
    )
    return-image
  )
)

;;laplace-po is similar to linear-blur-po
(defn laplace-po [ori-image re-image x y]
  (let [grey-matrix (get-grey-matrix ori-image x y)
        grey (int (two-vec-muladd grey-matrix [0.25 0.5 0.25 0.5 -3 0.5 0.25 0.5 0.25]))]
    (set-grey re-image x y grey)
    re-image
  )
)

;;laplace is similar to blur method
(defn laplace [filename]
  (let [image (read-image filename)
         return-image (new-image (get-width image)(get-height image))]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (smooth-po image return-image x y)
      )
    )
    return-image
  )
)

;;Get the q1, q2(median) and q3 points of whole pixels
(defn Q123 [image]
  (let [pixels (atom [])]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (swap! pixels conj (get-grey image x y))
      )
    )
    ;;get the q1, q2(median), q3 points being in a vector
    [(nth (sort (remove nil? @pixels)) (int (* (count @pixels) 0.25)))
     (nth (sort (remove nil? @pixels)) (int (* (count @pixels) 0.5)))
     (nth (sort (remove nil? @pixels)) (int (* (count @pixels) 0.75)))
     ]
  )
)

;;threshold-po is to check the pixel going which value, q1 or q3, by threshold q2
(defn threshold-po [image re-image x y q1 q2 q3]
  (let [greyscale (get-grey image x y)]
    ;;threshold intensity value into that pixel
    (cond
      (nil? greyscale) (set-grey re-image x y q1)
      (< greyscale q2) (set-grey re-image x y q1)
      (> greyscale q2) (set-grey re-image x y q3)
      :else (set-grey re-image x y q1)
    )
    re-image
  )
)

;;Main method to call it
(defn threshold [filename]
  (let [image (read-image filename)
        return-image (new-image (get-width image)(get-height image))
        Q123-value (Q123 image)]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (threshold-po image return-image x y (first Q123-value) (second Q123-value) (last Q123-value))
      )
    )
    return-image
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))