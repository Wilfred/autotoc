(ns autotoc.core
  (:require [clojure.string :refer [split-lines trim join lower-case]]
            [clojure.string :as str])
  (:import [java.io File]))

(defn- get-headings
  "Return a vector of headings in the given markdown source."
  [markdown]
  (loop [headings [] lines (split-lines markdown) in-code false]
    (let [line (first lines)]
      (cond
       (empty? lines) headings
       (.startsWith line "#") (recur (conj headings line) (rest lines) in-code)
       :else (recur headings (rest lines) in-code)))))

(defn- add-weight
  "Strip # characters from a heading and return a list (weight, text-content)."
  [heading]
  (let [[_ prefix text] (re-find #"^(#+)(.*?)#*$" (trim heading))]
    (list (count prefix) (trim text))))

(defn- repeat-string
  "Return a string that is the original string repeated n times."
  [string n]
  (apply str (repeat n string)))

(defn- text->link
  "Given heading text, convert it to a slug in the form of links used by GitHub."
  [heading]
  (-> heading
      lower-case
      (str/replace #"[^a-z0-9 ]" "")
      (str/replace " " "-")))

(defn- build-toc-tree
  "Return a markdown nested list of bullets for this table of contents."
  [weighted-headings]
  (join "\n"
        (map (fn [[weight text]]
               (format "%s- [%s](#%s)"
                       (repeat-string "  " (dec weight))
                       text
                       (text->link text)))
             weighted-headings)))

(defn build-toc
  "Given markdown source return a table of contents."
  [markdown]
  (format
   "**Table of Contents** *generated with autotoc*\n\n%s\n\n"
   (->> markdown
        get-headings
        (map add-weight)
        build-toc-tree)))

(defn- remove-toc
  "Remove an existing table of contents from markdown source."
  [markdown]
  (trim
   (str/replace
    markdown
    ;; i: case insensitive, m: ^ matches newlines anywhere in string
    #"(?im)\**table of contents.*

(\s*- \[.*\]\(.*\)\n)*"
   "")))

(defn- update-toc
  "Add or update a table of contents in markdown source."
  [markdown]
  (->> markdown
       remove-toc
       (str (build-toc markdown))))

(defn- file-exists?
  [path]
  (.exists (File. path)))

(defn -main
  "I don't do a whole lot."
  [& filenames]
  (if filenames
    (doseq [filename filenames]
      (if (file-exists? filename)
        (spit filename (update-toc (slurp filename)))
        (println "No such file:" filename)))
    (println "Usage: /path/to/readme.md")))
