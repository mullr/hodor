(ns hodor.core-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer :all]
            [hodor.core :refer :all]))

(deftest arithmetic
  (testing "simple addition"
    (are [exp val] (= (compile-and-run exp) val)
         '(+ 10 15) "25"
         '(+ -10 15) "5")))
