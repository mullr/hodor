(ns hodor.core-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer :all]
            [hodor.core :refer :all]))

(deftest arithmetic
  (testing "immediates"
    (are [exp val] (= (compile-and-run exp) val)
         0 "0"
         42 "42"
         \a "a"
         \Z "Z"
         :empty-list "()"
         'false "false"
         'true "true"))

  (testing "unary primitives"
    (are [exp val] (= (compile-and-run exp) val)
         '(zero? 0) "true"
         '(zero? 1) "false"
         '(zero? \a) "false"
         '(zero? false) "false"
         '(zero? :empty-list) "false"

         '(null? :empty-list) "true"
         '(null? 0) "false"
         '(null? \a) "false"
         '(null? false) "false"

         '(integer? 0) "true"
         '(integer? 42) "true"
         '(integer? \a) "false"
         '(integer? false) "false"
         '(integer? :empty-list) "false"

         '(boolean? true) "true"
         '(boolean? false) "true"
         '(boolean? 42) "false"
         '(boolean? \a) "false"
         '(boolean? :empty-list) "false"
         )))
