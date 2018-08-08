(ns lab-3.core
  (:gen-class))


(defn create-matrix [rows cols]
  (vec (replicate rows
          (vec (replicate cols 0)
          )
       )
  )
)

(defn get-dims-matrix [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (hash-map :nrows rows, :ncols cols)
  )
)

(defn get-matrix-item [matrix row col]
  (get-in matrix [row col])
)

(defn get-matrix-row [matrix row]
  (nth matrix row)
)

(defn get-matrix-col [matrix col]
  (let [{row :nrows}(get-dims-matrix matrix)]
    (loop [return-col (vector)
           row-counter 0]
      (if (= row-counter row)
        ;true?
        return-col
        ;false?
        (recur (conj return-col (get-matrix-item matrix row-counter col))
               (+ row-counter 1)
        )
      )
     )
  )
)

(defn set-matrix-item [matrix row col value]
  (assoc-in matrix [row col] value)
)

;This below solution is from https://stackoverflow.com/a/18939759
(defn query-matrix [data item]
  (let [n (count (first data))
        i (.indexOf (flatten data) item)]
    (if (pos? i)
      (vector (quot i n) (mod i n))))
)

(def test-matrix (create-matrix 4 4))
(get-dims-matrix test-matrix)
(get-matrix-item test-matrix 1 1)
(get-matrix-row test-matrix 2)
(get-matrix-col test-matrix 1)
(set-matrix-item test-matrix 0 0 3)
(query-matrix test-matrix 3)




(for [row (range 4)]
  (for [col (range 4)]
    (println row col)
  )
)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [initial-matrix (create-matrix 4 3)
        modified-matrix (-> (set-matrix-item initial-matrix 1 2 99)
                            (set-matrix-item 0 0 82)
                            (set-matrix-item 2 0 42)
                            (set-matrix-item 3 1 1)
                            (set-matrix-item 1 1 "hi")
                        )]

    (println "initial matrix:" )
    (println initial-matrix)

    (println "modified matrix:")
    (println modified-matrix)

    (println "dimensions of modified matrix: ")
    (println (get-dims-matrix modified-matrix))

    (println "row 1 of the matrix:")
    (println (get-matrix-row modified-matrix 1))

    (println "col 0 of the matrix:")
    (println (get-matrix-col modified-matrix 0))

    (println "col 2 of the matrix:")
    (println (get-matrix-col modified-matrix 2))

    (println "query the location of 42")
    (println (query-matrix modified-matrix 42))

    (println "query the location of hi")
    (println (query-matrix modified-matrix "hi"))

    (println "query the location of 99")
    (println (query-matrix modified-matrix 99))

    (println "query the location of 37333")
    (println (query-matrix modified-matrix 37333))
  )
)
