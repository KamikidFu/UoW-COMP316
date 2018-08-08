(ns as2.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
)

;;Author: Yunhao FU
;;ID: 1255469
;;Using git to control versions and stored in a private repository

;;A simple method to reload the project in the repl
(defn reload []
  (use 'as2.core :reload)
  :RELOADED!
)

;============================================================================================================================
;PART1
;============================================================================================================================

;;A supporting method to read file one line by one line
(defn read-line-by-line [filename]
    (with-open [reader (io/reader filename)] ;open file with reader of clojure.java.io
        (into [] (line-seq reader)) ;read a line from buffer as lazy sequence and put into list
    )
)

;;A map formatter method. This is going to re-format the map to what it should be in my state
(defn format-map [raw-map]
  (loop [processing-map raw-map ;Recursively process the map
         formatted-map []] ;A return value of formatted map
    (if (empty? processing-map) ;If the processing map is empty
      formatted-map ;Return the formatted map
      (recur (rest processing-map) ;Otherwise do next recur of formatting
             (conj formatted-map (into [] (string/replace (first processing-map) #" " "_")));Basically formatting the empty space to the underline
      )
    )
  )
)

;;A method to find item's position, well, not just s for Start position.
;;THIS IS A REQUIRED FUNCTION, start and goal
(defn find-item-s-pos [game-map in-game-item]
  (let [formatted-map game-map ;game-map should be a formatted map
        n (count (first formatted-map)) ;get the length of one row
        i (.indexOf (flatten formatted-map) in-game-item)] ;then flatten the map and check the index of the checking element.
    (if (pos? i) ;Do a math here
      (vector (quot i n) (mod i n))) ; return the position as a vector
  )
)

;;A method to read map from file
;;THIS IS A REQUIRED FUNCTION
(defn read-map-from-file [filename]
  (let [formatted-map (format-map (read-line-by-line filename))];Local variable is a formatted-map. Which value is returned from firstly reading line and then being formatted
    {
      :map formatted-map ;map keyword is the formatted map
      :path [] ;an empty path initially
      :start-pos (find-item-s-pos formatted-map \S) ;the start position
      :goal-pos (find-item-s-pos formatted-map \G) ;and the goal position
    }
  )
)

;;A method to draw a star * where the ship visited at pos - position
(defn ship-visit [state-map pos]
  (let [x-pos (first pos) ;get the x position
        y-pos (second pos)] ;and the y position
      (if (= (get-in state-map [x-pos y-pos]) \G) ;Check if it is goal
        (assoc-in state-map [x-pos y-pos] \G) ;if so, leave the G symbol there
        (assoc-in state-map [x-pos y-pos] \*) ;otherwise, change it to *
      )
  )
)

;;A method to shift the current position based on path value
(defn shift-pos [path pos]
  (let [x-pos (first pos) ;get the x position
        y-pos (second pos)] ;get the y position
        (case path ;switch case syntax to nicely shift position
           :n (vector (dec x-pos) y-pos) ;if it is :n, north, decrease x position
           :s (vector (inc x-pos) y-pos) ;if it is :s, south, increase x position
           :w (vector x-pos (dec y-pos)) ;if it is :w, west, decrease y position
           :e (vector x-pos (inc y-pos)) ;if it is :e, east, increase y position
        )
   )
)

;;A method to update map, that is, based on the vector of path, changing the position of ship and draw the * symbol where it visited
(defn update-map [state]
 (let [{current-map :map current-path :path start-pos :start-pos}state] ;deconstruct the state to get the map, path and start position
    (loop [processing-map current-map ;get the initially processing map
           processing-path current-path ;get the initially processing path
           processing-pos start-pos] ;get the initially processing position
      (if (empty? processing-path) ;if the processing path is empty
        processing-map ;return the current processing map
        (recur (ship-visit processing-map (vector (first (shift-pos (first processing-path) processing-pos)) ;recur the next stage map, that is, based on the first item in path, update the map
                                                  (second (shift-pos (first processing-path) processing-pos))))
               (rest processing-path) ;rest of processing path
               (shift-pos (first processing-path) processing-pos) ;shift the current position to next stage
        )
      )
    )
 )
)



;;A method to print the state
;;THIS IS A REQUIRED FUNCTION
(defn print-state [state]
    (loop [ready-to-print (update-map state)] ;do a loop to print each vector data in the vector of map, that is, print the vector of vectors
      (if-not (= (first ready-to-print) nil) ;if there still has any item to print
        (println (first ready-to-print)) ;then print the vector
      )
      (if (empty? ready-to-print) ;check if it is empty
        :PRINTED ;if so, print the label of :PRINTED
        (recur (rest ready-to-print)) ;otherwise, recur to next stage
      )
    )
)

;;A method to get the current position based on path
;;THIS IS A REQUIRED FUNCTION
(defn position [state]
  (let [{start-pos :start-pos current-path :path}state] ;deconstruct the state to get start position and current path
    (if (empty? current-path) ;if the current path is empty
      start-pos ;then directly output the start position
      (loop [processing-path current-path ;otherwise do a loop to check the position
             processing-pos start-pos] ;give it two values of path and position
        (if (empty? processing-path) ;if the path is empty
          processing-pos ;then return the current position
          (recur (rest processing-path) ;otherwise do recur to next stage
                 (shift-pos (first processing-path) processing-pos) ;first argument is the rest of path, and second is to shift the position
          )
        )
      )
    )
  )
)

;;A method to check if it is goal state
(defn goal? [state]
  (let [{goal-pos :goal-pos}state ;deconstruct the state to get the position of goal and current position
        current-pos (position state)]
    (= current-pos goal-pos) ;check if they are equal
  )
)

;;A method to get the cost of current state
;;THIS IS A REQUIRED FUNCTION
(defn cost [state]
  (let [{paths :path}state] ;deconstruct the state to get path
    (count paths) ;return the length of path
  )
)

;;A method to calculate heuristic value
;;THIS IS A REQUIRED FUNCTION
(defn heuristic [state]
  (let [{goal-pos :goal-pos}state
        ship-pos (position state)
        goal-x (first goal-pos)
        goal-y (second goal-pos)
        ship-x (first ship-pos)
        ship-y (second ship-pos)
        delta-x (- goal-x ship-x)
        delta-y (- goal-y ship-y)] ;after deconstructing goal from state, calculate all useful information to do the math of heuristic
    (Math/sqrt (+ (* delta-x delta-x)(* delta-y delta-y))) ;return the heuristic, the Euclidean distance
  )
)

;;A method to get the item, that is the symbol, at a certain position in map
(defn get-item-at-pos [state pos]
  (let [{state-map :map}state ;deconstruct the state to get the map
        x (first pos) ;get the x value
        y (second pos)] ;get the y value
    (get-in state-map [x y]) ;use get-in to get the item at position x and y.
  )
)

;;A method to update state based on update-map method
(defn update-state [state adding-path]
  (let [{current-path :path}state ;deconstruct path from the state
        returning-state (assoc state :path (conj current-path adding-path))] ;set a value of returning-state
    (assoc returning-state :map (update-map returning-state)) ;do assoc, make the new map into the returning-state
  )
)

;;A method to expand current position to other four (maybe four or less) position, north, south, east and west
(defn expand [state]
  (let [updated-state (assoc state :map (update-map state))
        current-pos (position updated-state)
        current-north  [(dec (first current-pos))  (second current-pos)]
        current-south [(inc (first current-pos)) (second current-pos)]
        current-east [(first current-pos) (inc (second current-pos))]
        current-west [ (first current-pos) (dec (second current-pos))]] ;first get the updated-state, then get the current position. Based on it, calculate other four position
    (loop [movement-set [current-north current-south current-east current-west]
           movement-flag [:n :s :e :w]
           returning-states []] ;use a loop to go though four position and check it one by one
      (if (empty? movement-set) ;if the movement-set is empty
        returning-states ;return the returning-states
        (recur (rest movement-set) ;do next recur
               (rest movement-flag)
               (let [icon (get-item-at-pos updated-state (first movement-set))] ;here is going to check the symbol
                 (if  (or (= icon \_) (= icon \G)) ;only \_ and \G are acceptable
                   (conj returning-states (update-state updated-state (first movement-flag))) ;if the item at that position is okay, then add it to the returning vector of states
                   returning-states ;or no operation here
                 )
               )
        )
      )
    )
  )
)


;============================================================================================================================
;PART2
;============================================================================================================================
;;Well, somehow it is required, but I did not use it.
(def verbose true)

;;A method to calculate best first algorithm value
(defn best-first-calculate-val [state]
  (heuristic state) ;call the heuristic method above
)

;;A method to format best first search state
(defn best-first-formatter [state]
    (loop [processing-states state
           returning-formatted []] ;do a loop here to maintain the best first state of lots of ship-map states
      (if (empty? processing-states) ;if it is empty
        returning-formatted ;return the formatted states
        (recur (rest processing-states) ;do recur to next stage
               (conj returning-formatted {:state (first processing-states) :val (heuristic (first processing-states)) :ship (position (first processing-states))}) ;formatting the state
        )
      )
    )
)

;;A method to check if this state is visited before
;;new expands are all new states that can be expanded
;;open-frontiers are all frontiers in the state
(defn check-visited [new-expands open-frontiers]
  (->> (concat new-expands open-frontiers) ;thread last macro to help checking though
           (group-by :ship) ; firstly concat two arguments together then group them by :ship
           vals ;get their value
           (map #(apply min-key :val %)) ;map to only get the minimun value of that state
  )
)

;;A method to do best first search
;;THIS IS A REQUIRED FUNCTION
(defn best-first [filename]
  (let [init-map (read-map-from-file filename)
        init-node {:state init-map :val (best-first-calculate-val init-map) :ship (position init-map)}] ;get the initial map and format it to a best first search node
    (loop [processing-frontier init-node ;do a loop here to go though the map
           open-frontiers (sort-by :val (best-first-formatter (expand init-map)))] ;maintain two arguments, one is the processing frontiers and all open frontiers
      (let [{current-state :state current-val :val current-ship :ship}processing-frontier] ;deconstruct the frontier to check its property below
        (if (goal? current-state) ;if this state is the goal
          (if-not (and (nil? processing-frontier) (> 10000 (count open-frontiers))) ;an if the processing-frontier is not nil
            (let [{state :state}processing-frontier] ;so, deconstruct the frontier to get the state value
              (print-state state);call the print-state function above
              (print "NUMBER OF EXPANDSIONS: " (count open-frontiers) "\n") ;print how many expandsions in the open frontiers
              (let [{path :path}state]
                (print "LENGTH OF PATH: " (count path) "\n") ;print how long the path is.
              )
              :FOUND_A_PATH ;return the keyword of :FIND_A_PATH
            )
            :CANNOT_FIND_A_PATH ;otherwise give the keyword of :CANNOT_FIND_A_PATH indicates cannot find a path
          )
          (recur (first open-frontiers) ;let's do the recur to next stage
                 ;TO-DO Check what expanded already included inside the closed-frontiers? If so, check the :val,
                 ;if :val is smaller, add to open-frontiers otherwise ignore.
                 (sort-by :val (distinct (check-visited     ;A method to check though all expanded spaces at current open-frontiers
                   (best-first-formatter   ;Format to node style
                     (expand current-state);Expanded to spaces
                   )
                   (rest open-frontiers)
                 )))
          )
        )
      )
    )
  )
)

;;A method to calculate the value basis in a* search
(defn a-star-calculate-val [state]
  (let [{state-path :path}state] ;deconstruct to get the path
    (+ (count state-path) (heuristic state)) ;count the path and add the heuristic of the state
  )
)

;;A method to format a-star search state
;;It is similar to best first formatter, because the things in the state are :state, the map and path stuff, :val the value basis, :ship current position of ship
(defn a-star-formatter [state]
  (if (= :map (first (first state)))
    {:state state :val (a-star-calculate-val state) :ship (position state)}
    (loop [processing-states state
           returning-formatted []]
      (if (empty? processing-states)
        returning-formatted
        (recur (rest processing-states)
               (conj returning-formatted {:state (first processing-states) :val (a-star-calculate-val (first processing-states)) :ship (position (first processing-states))})
        )
      )
    )
  )
)

;;A method to do a-star
;;THIS IS A REQUIRED FUNCTION
;;It is similar to best first search, yes, it is! Only difference is using a-star formatter
(defn a-star [filename]
  (let [init-map (read-map-from-file filename)
        init-node {:state init-map :val (a-star-calculate-val init-map) :ship (position init-map)}]
    (loop [processing-frontier init-node
           open-frontiers (sort-by :val (a-star-formatter (expand init-map)))]
      (let [{current-state :state current-val :val}processing-frontier]
        (if (goal? current-state)
          (if-not (nil? processing-frontier)
            (let [{state :state}processing-frontier]
              (print-state state)
              (print "NUMBER OF EXPANDSIONS: " (count open-frontiers) "\n")
              (let [{path :path}state]
                (print "LENGTH OF PATH: " (count path) "\n") ;print how long the path is.
              )
              :FOUND_A_PATH
            )
            :CANNOT_FIND_A_PATH
          )
          (recur (first open-frontiers)
                 ;TO-DO Check what expanded already included inside the closed-frontiers? If so, check the :val,
                 ;if :val is smaller, add to open-frontiers otherwise ignore.
                 (sort-by :val (distinct (check-visited     ;A method to check though all expanded spaces at current open-frontiers
                   (a-star-formatter   ;Format to node style
                     (expand current-state);Expanded to spaces
                   )
                   (rest open-frontiers)
                 )))
          )
        )
      )
    )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
)
