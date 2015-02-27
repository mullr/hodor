(ns hodor.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def fixnum-mask   0x3)
(def fixnum-tag    0x0)
(def fixnum-shift  2)

(def char-mask     0xff)
(def char-tag      0x0f)
(def char-shift    8)

(def bool-mask     0x7f)
(def bool-tag      0x3f)
(def bool-shift    7)

(def empty-list    47)

;;;;;;;;;; Immediates
(defn immediate-rep [x]
  (cond
    (integer? x) (bit-shift-left x fixnum-shift)
    (char? x) (-> (int x) (bit-shift-left char-shift) (bit-or char-tag))
    (= java.lang.Boolean (type x))  (-> (if x 1 0)
                                        (bit-shift-left bool-shift)
                                        (bit-or bool-tag))
    (= x :empty-list) empty-list
    :else nil))

(defn immediate? [x]
  (not (nil? (immediate-rep x))))

;;;;;;;; Primitive assembly dispatch
(defn dispatch-primcall [x] (if (seq x) (first x) :default))
(defmulti compile-primcall dispatch-primcall)
(defmethod compile-primcall :default [_] nil)
(defn primcall? [x] (not (nil? (dispatch-primcall x))))

(declare compile-expr)

;;;;;;;; Primitives
(defmethod compile-primcall 'inc [[_ x]]
  (concat (compile-expr x)
          [["addl" (str "$" (immediate-rep 1)) "%eax"]]))

(defmethod compile-primcall 'dec [[_ x]]
  (concat (compile-expr x)
          [["addl" (str "$" (immediate-rep -1)) "%eax"]]))

(defmethod compile-primcall 'integer->char [[_ x]]
  (concat (compile-expr x)
          [["shl" "$6" "%eax"]
           ["xor" "$0x3f" "%eax"]]))

(defmethod compile-primcall 'char->integer [[_ x]]
  (concat (compile-expr x)
          [["shr" "$6" "%eax"]]))

(defn compare-val [val]
  [["cmpl" (str "$" val) "%eax"] ; Set ZF to 1 if eax is our value
   ["movl" "$0" "%eax"]          ; Zero out eax
   ["sete" "%al"]                ; Set the low byte of eax to 1 if the ZF flag is set
   ["sall" "$7" "%eax"]          ; Convert eax to a tagged boolean
   ["orl" "$63" "%eax"]])

(defmethod compile-primcall 'zero? [[_ x]]
  (concat (compile-expr x)
          (compare-val (immediate-rep 0))))

(defmethod compile-primcall 'null? [[_ x]]
  (concat (compile-expr x)
          (compare-val (immediate-rep :empty-list))))

(defmethod compile-primcall 'integer? [[_ x]]
  (concat (compile-expr x)
          [["andl" (str "$" fixnum-mask) "%eax"]]
          (compare-val fixnum-tag)))

(defmethod compile-primcall 'boolean? [[_ x]]
  (concat (compile-expr x)
          [["andl" (str "$" bool-mask) "%eax"]]
          (compare-val bool-tag)))

;;;;;;;;; Compiler
(defn compile-expr [x]
  (cond
    (immediate? x) [["movl" (str "$" (immediate-rep x)) "%eax"]]
    (primcall? x) (compile-primcall x)))

(defn asm-vec-to-line [asm-vec]
  (let [[op & args] asm-vec]
    (str op " " (str/join ", " args))))

(def prefix
 [".section __TEXT,__text,regular,pure_instructions"
  ".globl _scheme_entry"
  ".align 4, 0x90"
  "_scheme_entry:"
  ".cfi_startproc"])

(def suffix
  ["retq"
   ".cfi_endproc"])

(defn compile [exp]
  (concat prefix
          (->> (compile-expr exp)
               (map asm-vec-to-line))
          suffix))

(defn assemble [asm]
  (spit "code.s" (str/join "\n" asm))
  (println (sh "cc" "code.s" "runner.c" "-o" "a.out")))

(defn compile-and-run [exp]
  (-> exp compile assemble)
  (-> (sh "./a.out") :out (str/trim-newline)))

(compile-and-run '(integer? 42))
