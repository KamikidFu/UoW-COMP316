(ns as1.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
)

;;Author: Yunhao FU
;;ID: 1255469
;;Using git to control versions and stored in a private repository
;;The time efficience as tested is around:
;;real	0m2.018s
;;user	0m2.270s
;;sys	  0m0.193s


;;----------------------------------------------------------------------------------------------
;;PART1
;;----------------------------------------------------------------------------------------------
;;count-word is a support method of create-multiset.
;;The main idea is similar to what we did in lab_2_5.
;;We count a single item in list and then record the number to somewhere.
;;Here, using assoc to directly record keyword and number to return-map.
;;Thanks to assoc not bind a new keyword to map, it checks if the keyword is existed in map, then assoc just changes its value.
;;Otherwise it bind the new pair of key and value to map.
;;Reference: https://clojuredocs.org/clojure.core/assoc#example-542692c9c026201cdc326acd
(defn count-word [return-map my-word]
  (assoc return-map
          my-word ;The keyword in map.
          (inc
            (get return-map my-word 0) ;Here using (get collection element default-return-value) function to get the current value of key in map.
          ) ; The value of that keyword before.
  )
)

;;With the handy function of count-word, here just using reduce to count-word step by step.
;;It returns a {} hash-map as I defined.
;;Reference: https://clojuredocs.org/clojure.core/reduce#example-542692ccc026201cdc326c39
(defn create-multiset [my-list]
  (reduce count-word {} my-list) ;Using reduce to call count-word function with a empty hash map and list of elements.
)

;;My implementation of size-multiset is to use the values of each key and sum them up.
;;A loop-recur structure is used.
;;The value of key is retrived by destructuring the map.
;;(defn size-multiset [my-set]
;;  (loop [size 0
;;        recur-set my-set] ;Declare the initial size, which is 0 and the initial set of doing recursion
;;    (if (empty? recur-set);If the set is empty...
;;      size                ;True? Then return the size
;;      (recur (let [[_ x](first recur-set)] ;False, do the next recursion
;;               (+ size x));Here is going to destructure the value of current key and add it up with previous size
;;             (rest recur-set);Pass the rest of set to next recursion
;;      )
;;    )
;;  )
;;)
;;The following is the new version of size-multiset, it is more straightforward. 
;;Using vals function to get all the values in the map, doing reduce function to add them one by one
(defn size-multiset [my-set]
  (reduce + (vals my-set))
) 

;;Intersecting two sets are kind of tricky since after implementing my solution, the test result looks good in part1 but not part2 when handling large set I guess.
;;My original solution is below:
;;The intersection function is going to check if the item in set 1 is also contained in set 2.
;;When the condition of containing is true, using assoc again to put keyword and the min counts to map.
;;The loop only goes though one set, so it is in linear efficience.
;;(defn intersect-multisets [set-1 set-2]
;;  (let [small-set (if (= (compare (size-multiset set-1) (size-multiset set-2)) 1)
;;                    set-2
;;                    set-1)
;;        longer-set (if (= small-set set-2)
;;                     set-1
;;                     set-2)]
;;    (loop [output {}
;;           refering-multiset small-set] ;output is a simple variable of empty hash map at beginning. Use set-2 as reference set.
;;      (if (empty?  refering-multiset) ;If the refering set is empty?
;;        output ;Then output the output of hash map of intersection
;;        (recur (let [[item-key item-count](first  refering-multiset)] ;else, do next recur. The first argument is deconstrucing from the refering set.
;;                 (when (contains? longer-set item-key) ;Check if the set-1 has the same key?
;;                   (assoc output item-key (min item-count (get longer-set item-key))) ;If so, using assoc to put the key and value of the minimun in those two set to output
;;                )
;;               )
;;               (rest  refering-multiset) ;Pass the rest of set to next recursion
;;        )
;;      )
;;    )
;;  )
;;)
;;
;;Therefore, I post a question with my idea and sample solution to StackOverFlow and hope someone could help me out of problem
;;The URL of question refers to https://stackoverflow.com/questions/49252956/intersection-of-two-hash-maps-using-clojure
;;This function below using select-keys and merge-with which I find it is fancy.
;;merge-with two collections with a function looks like a good way to shape new collection (Refet to https://clojuredocs.org/clojure.core/merge-with)
;;select-keys is going to pick out the selected keys from collection with its value (Refer to https://clojuredocs.org/clojure.core/select-keys)
;;This idea and implementation of function credited by Taylor Wood and Michiel Borkent
;;(defn intersect-multisets [set-1 set-2]
;; (select-keys (merge-with min set-1 set-2)
;;               (clojure.set/intersection
;;                 (set (keys set-1))
;;                 (set (keys set-2))
;;               )
;;  )
;;)
;;
;;But there is the other solution provided by other clojurist and I tested both solution find this one is quicker than the one above.
;;Both functions avoid using loop-recur structure and somehow now I believe it is the Zen of clojure language itself.
;;This solution is using reduce-kv, an enhanced reduce built-in function to especially hanlde key and value in collection.
;;reduce-kv kindly refers to https://clojuredocs.org/clojure.core/reduce-kv
;;The structure of reduce-kv is (reduce-kv function initial collection), the function basically contains 3 arguments which are map, key and value.
;;The idea of this function is quite similar to mine, both using one set of elements to check though the other one. If it contains the key, put into output map with value, otherwise go next key.
;;This function creadited by leetwinski
(defn intersect-multisets [set-1 set-2]
  (reduce-kv (fn [output k v]
               (if (contains? set-1 k) ;Is the key contained in set-1?
                 (assoc output k (min (set-1 k) v));Yes, put the key into output map with its minimun value
                 output ;Or, just return the previous output
               )
             );The function part
             {};The initial of output
             set-2;The collection to refer
  )
)


;;----------------------------------------------------------------------------------------------
;;PART2
;;----------------------------------------------------------------------------------------------
;;Reading line by line is kind of standard IO operation in clojure.
;;Using dependency of reader in clojure.java.io, open the reader and it will do reading works for you.
;;Here is going to put each word in one line into a big list.
(defn read-line-by-line [filename]
    (with-open [reader (io/reader filename)] ;open file with reader of clojure.java.io
        (into '() (line-seq reader)) ;read a line from buffer as lazy sequence and put into list
    )
)

;;read-line-by-line-and-split is similar to read-line-by-line above, the difference is the splitting function used here.
;;After read a lazy-seq from reader buffer, apply them to string and then split into elements.
;;Using into to put the value into a list
(defn read-line-by-line-and-split [filename]
    (with-open [reader (io/reader filename)]
      (into '() (string/split
                  (apply str
                         (line-seq reader);Read a line from reader
                  );Make the lazy-seq to become a string
                  #" ";Split with space
                )
      );Put value into a list
    );Open file for reading
)

;;This is a required function in assignment, simply having an arugment of filename.
;;Call the read-line-by-line function first, then create-multiset using the returned list.
(defn create-multiset-from-text-file [filename]
  (create-multiset (read-line-by-line filename));Read the file first, then create multiset.
)

;;This is also a required function in assignment.
;;Analysing file with pos and neg dictionary.
;;Frankly call the functions we implemented above and it is fine.
(defn analyse-sentiment [pos-set neg-set filename]
  (let [file-set (create-multiset (read-line-by-line-and-split filename))
        pos-score (size-multiset (intersect-multisets pos-set file-set))
        neg-score (size-multiset (intersect-multisets neg-set file-set))
        sentiment (- pos-score neg-score)];Define "local variable" using let. Each variable call its function step by step.
    ;Print the outcome one by one.
    (println "file" filename)
    (println "  positive score " pos-score)
    (println "  negative score " neg-score)
    (println "  final sentiment " sentiment)
  )
)





;;----------------------------------------------------------------------------------------------
;;TESTING PART
;;----------------------------------------------------------------------------------------------
;;Test function of part1 offered in assignment
(defn part1 []
  (let [a (create-multiset
            '("ab" "cd" "ef" "ab" "fg" "ef" "ab" "dd"))
        b (create-multiset
            '("zz" "ef" "fg" "ef" "ab" "ab" "ef" "ef"))
        c (intersect-multisets a b)]
    (println " a = " a ", size = " (size-multiset a))
    (println " b = " b ", size = " (size-multiset b))
    (println " c = " c ", size = " (size-multiset c))
  )
)

;;Test function of part2 offered in assignment
(defn part2 []
  (let [pos (create-multiset-from-text-file "positive-words.txt")
        neg (create-multiset-from-text-file "negative-words.txt")]
    (println "size of pos dictionary = " (size-multiset pos))
    (println "size of neg dictionary = " (size-multiset neg))
    (analyse-sentiment pos neg "pos1.txt")
    (analyse-sentiment pos neg "pos2.txt")
    (analyse-sentiment pos neg "pos3.txt")
    (analyse-sentiment pos neg "neg1.txt")
    (analyse-sentiment pos neg "neg2.txt")
    (analyse-sentiment pos neg "neg3.txt")
  )
)


;;Put part1/part2 function in main
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (part1)
  (part2)
)

;;----------------------------------------------------------------------------------------------
;;REFERENCE in APA Format
;;----------------------------------------------------------------------------------------------
;;ClojureDocs. (n.d.). assoc - clojure.core. Retrieved from ClojureDocs - Community-Powered Clojure Documentation and Examples: https://clojuredocs.org/clojure.core/assoc#example-542692c9c026201cdc326acd
;;ClojureDocs. (n.d.). merge-with - clojure.core. Retrieved from ClojureDocs - Community-Powered Clojure Documentation and Examples: https://clojuredocs.org/clojure.core/merge-with
;;ClojureDocs. (n.d.). reduce - clojure.core. Retrieved from ClojureDocs - Community-Powered Clojure Documentation and Examples: https://clojuredocs.org/clojure.core/reduce#example-542692ccc026201cdc326c39
;;ClojureDocs. (n.d.). reduce-kv - clojure.core. Retrieved from ClojureDocs - Community-Powered Clojure Documentation and Examples: https://clojuredocs.org/clojure.core/reduce-kv
;;ClojureDocs. (n.d.). select-keys - clojure.core. Retrieved from ClojureDocs - Community-Powered Clojure Documentation and Examples: https://clojuredocs.org/clojure.core/select-keys
;;Kamikid, Woord, T., Borkent, M., & leetwinski. (2018, 3 13). Intersection of two hash-maps using clojure. Retrieved from Stack Overflow: https://stackoverflow.com/questions/49252956/intersection-of-two-hash-maps-using-clojure