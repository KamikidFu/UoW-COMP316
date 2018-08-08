(ns lab2-6.core
  (:gen-class))

(defn my-reverse [my-list]
    (loop [my-return-list (list)
           processing-list my-list]

      (if (empty? processing-list)
        ;true? then return
        my-return-list
        ;false? else do recur
        (recur
          (conj my-return-list (first processing-list))
          (rest processing-list)
        )
      )

    )
)

(defn new-reverse [my-list]
  (into '() my-list)
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (my-reverse '(1 2 3 4 5)))
  (println (new-reverse '(1 2 3 4 5)))
)




