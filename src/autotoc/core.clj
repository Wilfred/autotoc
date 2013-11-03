(ns autotoc.core
  (:require [clojure.string :refer [split-lines]]))

(defn get-headings
  "Return a vector of headings in the given markdown source."
  [markdown]
  (loop [headings [] lines (split-lines markdown) in-code false]
    (let [line (first lines)]
      (cond
       (empty? lines) headings
       (.startsWith (trim line) "#") (recur (conj (trim headings) line) (rest lines) in-code)
       :else (recur headings (rest lines) in-code)))))

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
