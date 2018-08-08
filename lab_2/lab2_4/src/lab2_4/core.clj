(ns lab2-4.core
  (:gen-class))

(defn print-strings-doseq [my-string]
  (doseq [my-char
            (apply vector my-string)
            :when (not (nil? my-char))
          ]
    (println my-char)
  )
)

(defn print-strings-recur [my-string]
  (if-not (empty? my-string)
    (do (println (first my-string))
      (recur (rest my-string))
    )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-strings-doseq "hello world!")
  (print-strings-recur "hello world!")
)
