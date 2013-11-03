(ns autotoc.core
  (:require [clojure.string :refer [split-lines trim]]))

(defn get-headings
  "Return a vector of headings in the given markdown source."
  [markdown]
  (loop [headings [] lines (split-lines markdown) in-code false]
    (let [line (first lines)]
      (cond
       (empty? lines) headings
       (.startsWith (trim line) "#") (recur (conj (trim headings) line) (rest lines) in-code)
       :else (recur headings (rest lines) in-code)))))

(defn add-weight
  "Strip # characters from a heading and return a list (weight, text-content)."
  [heading]
  (let [[_ prefix text] (re-find #"^(#+)(.*?)#*$" (trim heading))]
    (list (count prefix) (trim text))))

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
