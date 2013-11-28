(ns autotoc.core-test
  (:require [clojure.test :refer :all]
            [autotoc.core :refer :all]))



(deftest no-headings
  (is (= (update-toc "foo" false) "foo"))
  (is (= (update-toc "\nfoo\n" false) "\nfoo\n"))
  (is (= (update-toc "```
#foo
```" false) "```
#foo
```")))

(deftest with-headings
  (is (= (update-toc "#foo" false)
         "**Table of Contents** *generated with [autotoc](https://github.com/Wilfred/autotoc)*

- [foo](#foo)

#foo")))
