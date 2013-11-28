(ns autotoc.core-test
  (:require [clojure.test :refer :all]
            [autotoc.core :refer :all]))



(deftest no-headings
  (is (= (update-toc "foo" false) "foo"))
  (is (= (update-toc "\nfoo\n" false) "\nfoo\n"))
  (let [hash-heading-in-code "```
#foo
```"]
    (is (= (update-toc hash-heading-in-code false)
           hash-heading-in-code)))
  (let [underline-heading-in-code "```
foo
===
```"
        underline-in-indented-code "   foo
   ---"]
    (is (= (update-toc underline-in-indented-code false)
           underline-in-indented-code))))

(deftest with-headings
  (is (= (update-toc "#foo" false)
         "**Table of Contents** *generated with [autotoc](https://github.com/Wilfred/autotoc)*

- [foo](#foo)

#foo"))
  (is (= (update-toc "foo\n===" false)
         "**Table of Contents** *generated with [autotoc](https://github.com/Wilfred/autotoc)*

- [foo](#foo)

foo
===")))
