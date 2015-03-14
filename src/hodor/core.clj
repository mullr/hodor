(ns hodor.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk postwalk]]
            [clojure.zip :as zip]))

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

(defn codegen-immediate [x]
  [["movl" x "%eax" ]])

;;;;;;;;; pseudo-assembly phases

(defn asm-zip
  "Construct a zipper that traverses both vectors and seqs, producing
  seqs when editing."
  [root]
  (zip/zipper #(or (seq? %) (vector? %))
              seq
              (fn [node children] (with-meta children (meta node)))
              root))

(defn prefixed-immediate-rep [x]
  (str "$" (immediate-rep x)))

(defn encode-immediates [asm-vec]
  (loop [loc (asm-zip asm-vec)]
    (cond
      (zip/end? loc) (zip/root loc)

      (immediate? (zip/node loc))
      (-> loc (zip/edit prefixed-immediate-rep) zip/next recur)

      :else (-> loc zip/next recur))))

(defn first-child [loc]
  (when (zip/branch? loc)
    (-> loc zip/down zip/node)))

(defn codegen-stack-ops [asm-vec]
  (loop [si 0, loc (asm-zip asm-vec)]
    (println "si" si)
    (cond
      (zip/end? loc) (zip/root loc)

      (= :push (first-child loc))
      (let [new-si (- si 4)]
        (recur new-si
               (zip/edit loc (fn [[_ source]]
                               ["movl" source (str new-si "(%esp)")]))))

      (= :stack (first-child loc))
      (recur si
             (zip/edit loc (fn [[_ relative-offset]]
                             (println "relative-offset" relative-offset)
                             (str (+ si (* 4 relative-offset)) "(%esp)"))))

      :else (recur si (zip/next loc)))))

;;;;;;;; Primitive assembly dispatch

(defn dispatch-primcall [x] (if (seq x) (first x) :default))
(defmulti codegen-primcall dispatch-primcall)
(defmethod codegen-primcall :default [_] nil)
(defn primcall? [x] (not (nil? (dispatch-primcall x))))

(defmacro defprim [sym args-vec & body]
  `(defmethod codegen-primcall (quote ~sym) [[_ ~@args-vec]]
     (concat ~@body)))

(declare codegen-expr)
;;;;;;;; unary primitives
(defprim inc [x]
  (codegen-expr x)
  [["addl" 1 "%eax"]])

(defprim dec [x]
  (codegen-expr x)
  [["addl" -1 "%eax"]])

(defprim integer->char [x]
  (codegen-expr x)
  [["shl" "$6" "%eax"]
   ["xor" "$0x3f" "%eax"]])

(defprim char->integer [x]
  (codegen-expr x)
  [["shr" "$6" "%eax"]])

;;;;;;;;; Unary predicates
(defn compare-eax [lit]
  [["cmpl" lit "%eax"]    ; Set ZF to 1 if eax is our value
   ["movl" "$0" "%eax"]   ; Zero out eax
   ["sete" "%al"]         ; Set the low byte of eax to 1 if the ZF flag is set
   ["sall" "$7" "%eax"]   ; Convert eax to a tagged boolean
   ["orl" "$63" "%eax"]])

(defprim zero? [x]
  (codegen-expr x)
  (compare-literal 0))

(defprim null? [x]
  (codegen-expr x)
  (compare-literal :empty-list))

(defprim integer? [x]
  (codegen-expr x)
  [["andl" (str "$" fixnum-mask) "%eax"]]
  (compare-literal fixnum-tag))

(defprim boolean? [x]
  (codegen-expr x)
  [["andl" (str "$" bool-mask) "%eax"]]
  (compare-literal (str "$" bool-tag)))

;;;;;;;;;;;; Binary primitives
(defprim + [a b]
  (codegen-expr a)
  [[:push "%eax"]]
  (codegen-expr b)
  [["addl" [:stack 0] "%eax"]])

(defprim - [a b]
  (codegen-expr b)
  [[:push "%eax"]]
  (codegen-expr a)
  [["subl" [:stack 0] "%eax"]])

;; Numbers are encoded, which makes this challenging. When encoded,
;; straight enc(a) * enc(b) = enc(4 * a * b)
;; So, we use b/4 to compensate
(defprim * [a b]
  (codegen-expr a)
  [[:push "%eax"]]
  (codegen-expr b)
  [["shr" "$2" "%eax"]
   ["imul" [:stack 0] "%eax"]])

(defprim = [a b]
  (codegen-expr a)
  [[:push "%eax"]]
  (codegen-expr b)
  (compare-literal [:stack 0]))

;;;;;;;;; Compiler
(defn codegen-expr [x]
  (cond
    (immediate? x) (codegen-immediate x)
    (primcall? x) (codegen-primcall x)))

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
  ["ret"
   ".cfi_endproc"])

(defn compile [expr]
  (let [asm (-> expr
                codegen-expr
                codegen-stack-ops
                encode-immediates)]
    (concat prefix
            (map asm-vec-to-line asm)
            suffix)))

(defn assemble [asm]
  (spit "code.s" (str/join "\n" asm))
  (println (sh "cc" "code.s" "runner.c" "-o" "a.out" "-m32")))

(defn compile-and-run [exp]
  (-> exp compile assemble)
  (-> (sh "./a.out") :out (str/trim-newline)))

#_(compile-and-run '(integer? 42))
