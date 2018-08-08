(ns lab2-5.core
  (:gen-class))

(defn count-single-char-in-string [my-string my-char]
  (count (re-seq my-char my-string))
)

(defn count-chars [my-string]
  {
    :a (count-single-char-in-string my-string #"a")
    :c (count-single-char-in-string my-string #"c")
    :t (count-single-char-in-string my-string #"t")
    :g (count-single-char-in-string my-string #"g")
  }
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (count-single-char-in-string "hello" #"l")
  (count-chars "aatgccacag")
)

