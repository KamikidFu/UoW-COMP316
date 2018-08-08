(ns lab2-1.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [even-list (filter even? (range 100 201))
        mul-seven (fn [x] (* x 7))]
    (println (apply sorted-set (map mul-seven even-list)))
  )
)

