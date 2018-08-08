(ns lab-5-2.core
  (:gen-class))

(defn init []
  #{}
)

(defn action [my-set item]
  (if (empty? my-set)
    (conj my-set item)
    (if (> item (last (apply sorted-set my-set)))
      (apply sorted-set (conj my-set item))
      (apply sorted-set my-set)
    )
  )
)

;(action #{1} 1)

;(action #{2 5} 7)
;(action #{2 7} 5)

(defn check-gene [gene-vec]
    (loop [return-set (init)
           processing-gene gene-vec
           counter 0]
      (if (empty? processing-gene)
        return-set
        (recur (if (= (first processing-gene) :Y)
                 (action return-set counter)
                 return-set
               )
               (rest processing-gene)
               (inc counter)
        )
      )
    )
)

;(check-gene [:Y :N :Y :N])

(defn read-file [filename]
  (read-string (slurp filename))
)

;(read-file "small.txt")

;(def first-one (first (read-file "small.txt")))

;(type (last first-one))

;(check-gene (subvec first-one 0 (- (count first-one) 1)))

(defn checking-gene [filename]
  (loop [gene-vec (read-file filename)
         tumour []
         not-tumour []]
    (if (empty? gene-vec)
      {:tumour tumour :not-tumour not-tumour}
      (recur (rest gene-vec)
             (if (= (last (first gene-vec)) :TUMOUR-YES)
                 (conj tumour (check-gene (first gene-vec)))
                 tumour
             )
             (if (= (last (first gene-vec)) :TUMOUR-NO)
               (conj not-tumour (check-gene (first gene-vec)))
               not-tumour
             )

      )
    )
  )
)

(checking-gene "small.txt")

;(clojure.set/intersection #{1 2 3 4 6 7 8 9 10 12 13 16 19 20 26 27 28} #{0 1 3 5 9 11 12 17 18 22 24 25 26 27 28 29})

(def tumour-gene-set (reduce clojure.set/intersection (second (first (checking-gene "small.txt")))))

(clojure.set/subset? tumour-gene-set (nth (second (second (checking-gene "small.txt"))) 0))

(defn checked-gene [filename]
  (let [gene-map (checking-gene filename)
        tumour-gene-set (reduce clojure.set/intersection (second (first gene-map)))]
    (loop [not-tumour-set (second (second gene-map))
           flag false]
      (if (empty? not-tumour-set)
        (if-not flag
          tumour-gene-set
          {}
        )
        (recur (rest not-tumour-set)
               (clojure.set/subset? tumour-gene-set (first not-tumour-set))
        )
      )
    )

  )
)

;(checked-gene "small.txt")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
