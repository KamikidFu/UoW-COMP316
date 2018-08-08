(ns lab2-2.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def seq-of-number (range 97 123))
  (def seq-of-alphabet
    (map keyword
         (map str
              (map char seq-of-number)
         )
    )
  )
  (println
    (apply sorted-map
           (interleave
             (apply list seq-of-alphabet)
             (apply list seq-of-number)
           )
    )
  )
)
