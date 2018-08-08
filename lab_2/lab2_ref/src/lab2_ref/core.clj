(ns lab2-ref.core
  (:gen-class))


(defn Q1 []
  (clojure.set/intersection
    (set (filter even?
                 (range 100 201)))
    (set (filter #(= 0 (mod % 7))
                 (range 100 201)))
  )
)

(defn Q2 []
  (let [ascii-codes (range 97 123)]
    (apply hash-map
           (interleave
             (map #(keyword (str (char %))) ascii-codes)
              ascii-codes
           )
    )
  )
)

(defn Q3 []
  (let [data [{:x 1 :y 2}{:x 6 :y 4}{:x 9 :y 7}]]
    (reduce + (map :x data))
  )
)

(defn Q4-print-string-doseq [my-strings]
  (doseq [string my-strings]
    (println string)
  )
)

(defn Q4-print-string-recur [my-string]
  (when-not (empty? my-string)
    (do (println (first my-string))
      (recur (rest my-string))
    )
  )
)

(defn Q5-count-single-char [string ch]
  (count (filter #(= % ch) string))
)

(defn Q5-count-chars-v1 [my-string]
  {
    :a (Q5-count-single-char my-string \a)
    :c (Q5-count-single-char my-string \c)
    :t (Q5-count-single-char my-string \t)
    :g (Q5-count-single-char my-string \g)
  }
)

(defn Q5-count-chars-v2
  ([my-string]
   (Q5-count-chars-v2 my-string {:a 0 :c 0 :t 0 :g 0})
  )
  ([remaining-string counts-so-far]
   (if (empty? remaining-string)
     counts-so-far (let [current-key (keyword (str (first remaining-string)))
                         current-count (get counts-so-far current-key)]
                     (recur
                       (rest remaining-string)
                       (assoc counts-so-far current-key (+ current-count 1))
                     )
                   )
   )
  )
)

(defn Q6-my-reverse
  ([my-list]
   (Q6-my-reverse my-list '())
  )
  ([remaining-list revered-so-far]
   (if (empty? remaining-list)
     revered-so-far
     (recur
       (rest remaining-list)
       (conj revered-so-far (first remaining-list))
     )
   )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println "Q1:")
  (println (Q1))
  (println "Q2:")
  (println (Q2))
  (println "Q3:")
  (println (Q3))
  (println "Q4: doseq")
  (println (Q4-print-string-doseq ["here" "are" "some" "strings"]))
  (println "Q4: recur")
  (println (Q4-print-string-recur ["here" "are" "some" "strings"]))
  (println "Q5: v1")
  (println (Q5-count-chars-v1 "aatgccacag"))
  (println "Q5: v2")
  (println (Q5-count-chars-v2 "aatgccacag"))
  (println "Q6:")
  (println (Q6-my-reverse '(1 2 3 4 5 6 7 8)))
)
