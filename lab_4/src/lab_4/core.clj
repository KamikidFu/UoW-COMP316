(ns lab-4.core
  (:gen-class))

(defn game-init []
  {:board "eeeeeeee", :player :red}
)

(defn get-initial-state []
  (game-init)
)

(defn check-board [board flag]
  (loop [rest-board board
         counter 0
         found 0]
    (if (= found 1)
      true
      ;false?
      (if-not (empty? rest-board)
        (recur (rest rest-board)
               (if (= (first rest-board) flag)
                 (+ counter 1)
                 0
               )
               (if (= counter 4)
                 1
                 0)
        )
        (if (= counter 4)
          true
          false
        )
      )
    )
  )
)
;(check-board "eeRRRRe" \R)

(defn game-goal [current-state]
  (let [{next-player :player current-board :board}current-state
         flag (if (= :red next-player) \B \R)]
    (check-board current-board flag)
  )
)
;(game-goal {:board "eeeeRRRR" :player :red})


(defn is-goal [state]
  (let [{next-player :player current-board :board}state]
    (if (game-goal state)
      (if (= next-player :red)
        "BBBB"
        "RRRR"
      )
      nil
    )
  )
)

(defn check-position [board position]
  (if (= (nth board position) \e)
    true
    false
  )
)
;(check-position "eeRRRRee" 2)

(defn make-action [board position player]
  (if (= player :red)
    (apply str (assoc (into [] board) position \R))
    (apply str (assoc (into [] board) position \B))
  )
)
;(make-action "eeeeRRRR" 2 :blue)

(defn action [state position]
  (let [{current-board :board next-player :player}state]
    (if (check-position current-board position)
      ;true
      (if (= next-player :red)
        (assoc state :player :blue :board (make-action current-board position :red))
        (assoc state :player :red :board (make-action current-board position :blue))
      )
      ;false
      nil
    )
  )
)
;(action {:board "eeeeRRRR" :player :blue} 4)

(defn check-state [state]
  (let [{current-board :board next-player :player}state]
    (loop [index-counter 0
           flag false]
      (if flag
        true
        (if (= index-counter 8)
          false
          (recur (inc index-counter)
                 (check-board (make-action current-board index-counter next-player) (if (= next-player :red) \R \B))
          )
        )
      )
    )
  )
)
;(check-state {:board "eeeeeeee" :player :blue})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Test of get-initial-state:")
  (println (get-initial-state))
  (println "Test of is-goal:")
  (println (is-goal {:board "eRRBBBBe" :player :red}))
  (println (is-goal {:board "eeReBeeB" :player :red}))
  (println "Tests of action:")
  (println (action {:board "eeeeeeee" :player :red} 0))
  (println (action {:board "BeeReeee" :player :red} 5))
  (println (action {:board "eeRRReee" :player :blue} 7))
  (println (action {:board "eeRRReee" :player :blue} 2))
  (println "Tests of check-state:")
  (println (check-state {:board "eeeeeeee" :player :blue}))
  (println (check-state {:board "ReRReBBB" :player :blue}))
  (println (check-state {:board "ReRReBBB" :player :red}))
  (println (check-state {:board "RBBBeeRR" :player :red}))
  (println (check-state {:board "RBBBeeRR" :player :blue}))
)
