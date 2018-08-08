(ns lab-5-1.core
  (:gen-class))

(defn action [state move]
  (if (= (count state) 9)
    nil
      (if (.contains state (str move))
        nil
          (apply str (flatten (conj (vec state) move)))
      )
  )
)

;;(action "123" 4)
;;(action "" 1)
;;(action "178" 7)
;;(action "123987456" 7)

;;(defn children [state]
;;  (let [full-state (map str (range 1 10))
;;        current-state (map str (apply list state))]
;;    (loop [remaining-state (remove #(.contains current-state %)  full-state)
;;           return-state-list '()]
;;      (if (empty? remaining-state)
;;        return-state-list
;;        (recur (rest remaining-state)
;;               (concat return-state-list (list (action current-state (first remaining-state))))
;;        )
;;      )
;;    )
;;  )
;;)

(defn children [state]
    (loop [remaining-state (range 1 10)
           return-state-list '()]
      (if (empty? remaining-state)
        (remove nil? return-state-list)
        (recur (rest remaining-state)
               (concat return-state-list (list (action state (first remaining-state))))
        )
      )
    )
)

;;(loop [my-list (range 1 10)
;;       counter 0]
;;  (if (empty? my-list)
;;    counter
;;    (recur (rest my-list)
;;          (inc counter))
;;  )
;;)


;(children "")
;(children "1")
;(children "12")
;(children "123")
;(children "1234")
;(children "12345")
;(children "123456")
;(children "1234567")
;(children "12345678")
;(children "123")
;(children "")
;(children "895412")
;(children "89542367")

;;(defn check-rows [state]
;; (let [rows (partition 3 (map str (flatten (partition 3 state))))]
;;    (loop [checking-rows rows
;;           flag false]
;;      (if (empty? checking-rows)
;;        flag
;;        (recur (rest checking-rows)
;;               (= 15 (reduce + (map #(Integer/parseInt %) (first checking-rows))))
;;        )
;;      )
;;    )
;;  )
;;)

(defn check-rows [state]
  (let [{a 0, b 1, c 2, d 3, e 4, f 5, g 6, h 7, i 8}(vec (map #(Integer/parseInt %) (map str state)))]
    (if (= 15 (+ a b c))
      (if (= 15 (+ d e f))
        (if (= 15 (+ g h i))
          true
          false
        )
        false
      )
      false
    )
  )
)
;(check-rows "123456789")
;(check-rows "276951438")

(defn check-cols [state]
  (let [{a 0, b 1, c 2, d 3, e 4, f 5, g 6, h 7, i 8}(vec (map #(Integer/parseInt %) (map str state)))]
    (if (= 15 (+ a d g))
      (if (= 15 (+ b e h))
        (if (= 15 (+ c f i))
          true
          false
        )
        false
      )
      false
    )
  )
)
;(check-cols "123456789")
;(check-cols "276951438")

(defn check-dias [state]
  (let [{a 0, b 1, c 2, d 3, e 4, f 5, g 6, h 7, i 8}(vec (map #(Integer/parseInt %) (map str state)))]
    (if (= 15 (+ a e i))
      (if (= 15 (+ c e g))
        true
        false
      )
      false
    )
  )
)

;(check-dias "123456789")
;(check-dias "276951438")

(defn is-goal [state]
  (if-not (= 9 (count state))
    false
    (if (check-rows state)
      (if (check-cols state)
        (if (check-dias state)
          true
          false
        )
        false
      )
      false
    )
  )
)

;(is-goal "123456789")
;(is-goal "276951438")

(defn search
  ([start-state]
    (search start-state (children start-state) 0 )
  )
  ([current-state open counter]
    (if (is-goal current-state)
      {:goal current-state :counter counter}
      (recur (first open)
             (concat (children (first open))
                     (rest open))
             (inc counter)))
  )
)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println(search ""))
)
