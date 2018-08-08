(ns lab2-3.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def data [{:x 1 :y 2}{:x 6 :y 4}{:x 9 :y 7}])
  (let [[{x1 :x}{x2 :x}{x3 :x}]data]
    (+ x1 x2 x3)
  )
)
