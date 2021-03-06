(ns autotoc.core
  (:require [clojure.string :refer [split-lines trim join lower-case]]
            [clojure.string :as str])
  (:import [java.io File])
  (:gen-class))

(defn- hash-heading?
  "Return true if this line of code is a markdown heading starting with #."
  [line]
  (.startsWith line "#"))

(defn- underline-heading?
  "Return true if these two lines make up a heading that is underlined."
  [line1 line2]
  (re-find #"^(=+|-+)\s*$" line2))

(defn- get-headings
  "Return a vector of headings in the given markdown source."
  [markdown]
  (let [index (ref 0)
        headings (ref [])
        in-code (ref false)
        lines (split-lines markdown)]
    (while (< @index (count lines))
      (let [line (lines @index)
            next-line (if (< @index (dec (count lines))) (lines (inc @index)))]
        (cond
         (and (hash-heading? line) (not @in-code))
         (dosync (alter headings #(conj % line)))

         (and
          next-line ;; not on the last line
          (underline-heading? line next-line)
          (not @in-code))
         (dosync
          (alter headings #(conj % (str line "\n" next-line)))
          (alter index inc))
         
         (.startsWith line "```")
         (dosync (alter in-code not))))
      (dosync (alter index inc)))
    @headings))

(defn- add-weight
  "Strip # characters from a heading and return a list (weight, text-content)."
  [heading]
  (if (hash-heading? heading)
    (let [[_ prefix text] (re-find #"^(#+)(.*?)#*$" (trim heading))]
      (list (count prefix) (trim text)))
    
    ;; otherwise, an underlined heading
    (let [[text underline] (split-lines heading)
          weight (if (re-find #"=+" underline) 1 2)]
      (list weight text))))

(defn- balance-weights
  "Given a (possibly empty) list of weighted headings, ensure the lowest weight is 1.
  FIXME: other ways the tree can be unbalanced"
  [weighted-headings]
  (if (seq weighted-headings)
    (let [lowest-weight
          (apply min (map first weighted-headings))
          excess (dec lowest-weight)]
      (if (zero? excess)
        weighted-headings
        ;; Subtract the same amount from all the headings to make the lowest weight 1.
        (map (fn [[weight heading]] [(- weight excess) heading]) weighted-headings)))
    weighted-headings))

(defn- repeat-string
  "Return a string that is the original string repeated n times."
  [string n]
  (apply str (repeat n string)))

(defn- github-text->link
  "Given heading text, convert it to a slug in the form of links used by GitHub."
  [heading]
  (format "#%s"
          (-> heading
              lower-case
              (str/replace #"[^a-z0-9 -]" "")
              (str/replace " " "-"))))

(defn- github-wiki-text->link
  "GitHub's wikis rewrite relative links, so we prepend a question mark."
  [heading]
  (format "?#%s"
          (-> heading
              lower-case
              (str/replace #"[^a-z0-9 -]" "")
              (str/replace " " "-"))))

(defn- extract-link-names
  "Replace all markdown links with just their names.
  E.g. \"[Foo Bar](http://example.com) bar\"
  becomes: \"Foo Bar bar\""
  [markdown]
  (str/replace markdown #"\[(.*?)\]\(.*?\)" (fn [[_ link-name]] link-name)))

(defn- build-toc-tree
  "Return a markdown nested list of bullets for this table of contents."
  [weighted-headings text->link]
  (join "\n"
        (map (fn [[weight text]]
               (format "%s- [%s](%s)"
                       (repeat-string "  " (dec weight))
                       (extract-link-names text)
                       (text->link (extract-link-names text))))
             weighted-headings)))

(defn build-toc
  "Given markdown source return a table of contents."
  [markdown wiki?]
  (let [tree (build-toc-tree
              (->> markdown
                   get-headings
                   (map add-weight)
                   balance-weights)
              (if wiki? github-wiki-text->link github-text->link))]
    (if (seq tree)
      (format
       "**Table of Contents** *generated with [autotoc](https://github.com/Wilfred/autotoc)*\n\n%s\n\n"
       tree)
      "")))

(defn- remove-toc
  "Remove an existing table of contents from markdown source."
  [markdown]
  (str/replace
   markdown
   ;; i: case insensitive, m: ^ matches newlines anywhere in string
   #"(?im)\**table of contents.*

(\s*- \[.*\]\(.*\)\n)*\n?"
   ""))

(defn update-toc
  "Add or update a table of contents in markdown source."
  [markdown wiki?]
  (->> markdown
       remove-toc
       (str (build-toc markdown wiki?))))

(defn- file-exists?
  [path]
  (.exists (File. path)))

(defn -main
  "Update the table of contents on all files specified."
  [& filenames]
  (let [wiki? (ref false)]
    (if filenames
      (doseq [filename filenames]
        (cond
         (= filename "--wiki")
         (dosync (ref-set wiki? true))

         (file-exists? filename)
         (spit filename (update-toc (slurp filename) @wiki?))

         :else
         (println "No such file:" filename)))
      (println "Usage: \n
autotoc <path>...
autotoc --wiki <path>..."))))
