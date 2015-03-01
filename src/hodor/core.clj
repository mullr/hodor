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
(defn dispatch-primcall [x si] (if (seq x) (first x) :default))
(defmulti compile-primcall dispatch-primcall)
(defmethod compile-primcall :default [_ _] nil)
(defn primcall? [x] (not (nil? (dispatch-primcall x 0))))

(declare compile-expr)

;;;;;;;; Primitives
(defmethod compile-primcall 'inc [[_ x] si]
  (concat (compile-expr x si)
          [["addl" (str "$" (immediate-rep 1)) "%eax"]]))

(defmethod compile-primcall 'dec [[_ x] si]
  (concat (compile-expr x si)
          [["addl" (str "$" (immediate-rep -1)) "%eax"]]))

(defmethod compile-primcall 'integer->char [[_ x] si]
  (concat (compile-expr x si)
          [["shl" "$6" "%eax"]
           ["xor" "$0x3f" "%eax"]]))

(defmethod compile-primcall 'char->integer [[_ x] si]
  (concat (compile-expr x si)
          [["shr" "$6" "%eax"]]))

(defn compare-literal [lit]
  [["cmpl" lit "%eax"] ; Set ZF to 1 if eax is our value
   ["movl" "$0" "%eax"]          ; Zero out eax
   ["sete" "%al"]                ; Set the low byte of eax to 1 if the ZF flag is set
   ["sall" "$7" "%eax"]          ; Convert eax to a tagged boolean
   ["orl" "$63" "%eax"]])

(defn compare-val [val]
  (compare-literal (str "$" val)))

;;;;;;;;; Unary predicates
(defmethod compile-primcall 'zero? [[_ x] si]
  (concat (compile-expr x si)
          (compare-val (immediate-rep 0))))

(defmethod compile-primcall 'null? [[_ x] si]
  (concat (compile-expr x si)
          (compare-val (immediate-rep :empty-list))))

(defmethod compile-primcall 'integer? [[_ x] si]
  (concat (compile-expr x si)
          [["andl" (str "$" fixnum-mask) "%eax"]]
          (compare-val fixnum-tag)))

(defmethod compile-primcall 'boolean? [[_ x] si]
  (concat (compile-expr x si)
          [["andl" (str "$" bool-mask) "%eax"]]
          (compare-val bool-tag)))

;;;;;;;;;;;; Binary primitives
(defmethod compile-primcall '+ [[_ a b] si]
  (let [si (- si 4)]
    (concat (compile-expr a si)
            [["movl" "%eax" (str si "(%esp)")]]
            (compile-expr b si)
            [["addl" (str si "(%esp)") "%eax"]])))

(defmethod compile-primcall '- [[_ a b] si]
  (let [si (- si 4)]
    (concat (compile-expr b si)
            [["movl" "%eax" (str si "(%esp)")]]
            (compile-expr a si)
            [["subl" (str si "(%esp)") "%eax"]])))


;; Numbers are encoded, which makes this challenging. When encoded,
;; straight enc(a) * enc(b) = enc(4 * a * b)
;; So, we use b/4 to compensate
(defmethod compile-primcall '* [[_ a b] si]
  (let [si (- si 4)]
    (concat (compile-expr a si)
            [["movl" "%eax" (str si "(%esp)")]]
            (compile-expr b si)
            [["shr" "$2" "%eax"]
             ["imul" (str si "(%esp)") "%eax"]])))

(defmethod compile-primcall '= [[_ a b] si]
  (let [si (- si 4)]
    (concat (compile-expr a si)
            [["movl" "%eax" (str si "(%esp)")]]
            (compile-expr b si)
            (compare-literal (str si "(%esp)")))))

(defmethod compile-primcall '< [[_ a b] si]
  ;; ????????????????
  []
)

;;;;;;;;; Compiler
(defn compile-expr [x si]
  (cond
    (immediate? x) [["movl" (str "$" (immediate-rep x)) "%eax"]]
    (primcall? x) (compile-primcall x si)))

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

(defn compile [exp]
  (concat prefix
          (->> (compile-expr exp 0)
               (map asm-vec-to-line))
          suffix))

(defn assemble [asm]
  (spit "code.s" (str/join "\n" asm))
  (println (sh "cc" "code.s" "runner.c" "-o" "a.out" "-m32")))

(defn compile-and-run [exp]
  (-> exp compile assemble)
  (-> (sh "./a.out") :out (str/trim-newline)))

#_(compile-and-run '(integer? 42))
