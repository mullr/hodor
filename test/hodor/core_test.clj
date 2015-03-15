(ns hodor.core-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer :all]
            [hodor.core :refer :all]
            [hodor.passes.codegen-stack-ops :refer [codegen-stack-ops]]
            [hodor.passes.encode-immediates :refer [encode-immediates]]))

(deftest encode-immediates-phase
  (are [in out] (= (encode-immediates in) out)
       [["movl" 1 "%eax"]] [["movl" "$4" "%eax"]]))

(deftest codegen-stack-ops-phase
  (is (= (codegen-stack-ops
          [["movl" 1 "%eax"]
           [:push "%eax"]
           ["movl" 2 "%eax"]
           [:push "%eax"]
           ["addl" [:stack 0] [:stack 1]]])

         [["movl" 1 "%eax"]
          ["movl" "%eax" "-4(%esp)"]
          ["movl" 2 "%eax"]
          ["movl" "%eax" "-8(%esp)"]
          ["addl" "-8(%esp)" "-4(%esp)"]])))

(deftest immediates
  (are [exp val] (= (compile-and-run exp) val)
       0 "0"
       42 "42"
       \a "a"
       \Z "Z"
       :empty-list "()"
       'false "false"
       'true "true"))

(deftest unary-primitives
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
       '(boolean? :empty-list) "false"))

(deftest binary-primitives
  (are [exp val] (= (compile-and-run exp) val)
       '(+ 1 1) "2"
       '(+ 1 0) "1"

       '(- 2 1) "1"
       '(- 1 0) "1"

       '(* 2 2) "4"
       '(* 1 0) "0"
       '(* -2 2) "-4"
       '(* 2 -2) "-4"
       '(* -2 -2) "4"

       '(= 1 1) "true"
       '(= 1 0) "false"))

(deftest let-test
  (are [exp val] (= (compile-and-run exp) val)
       '(let [a 42] a) "42"
       '(let [a 1] (let [b 2] (+ a b))) "3"
       '(let [a 1] (let [a 2] a)) "2"))

(deftest if-test
  (are [exp val] (= (compile-and-run exp) val)
       '(if true 1 2) "1" 
       '(if false 1 2) "2"
       '(if (= 1 1) 1 2) "1"
       '(if (= 1 2) 1 2) "2"))


