(ns autotoc.core-test
  (:require [clojure.test :refer :all]
            [autotoc.core :refer :all]))

(deftest no-headings
  (is (= (update-toc "foo" false) "foo"))
  (is (= (update-toc "\nfoo\n" false) "\nfoo\n")))
