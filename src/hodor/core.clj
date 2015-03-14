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

;;;;;;;;;; Compiler state threading
(def empty-state
  {:si 0
   :code []
   :call-depth 0})

(defn thread-state [& fns]
  (fn [state]
    (loop [state state, fns fns]
      (if (seq fns)
        (let [state (update-in state [:call-depth] inc)
              state ((first fns) state)
              state (update-in state [:call-depth] dec)]
          (recur state (rest fns)))
        state))))

(defn add-instr [s & is]
  (update-in s [:code] concat is))

(defn instr [& is]
  (fn [s]
    (update-in s [:code] concat is)))

(defn asm-comment [& strs]
  (fn [s]
    (let [prefix (apply str (repeat (:call-depth s) "#"))
          comment-str (str/join " " (cons prefix strs))]
      (update-in s [:code] concat [[comment-str]]))))

(defn push []
  (fn [s]
    (let [si (:si s)
          new-si (- si 4)]
     (-> s
         (add-instr ["movl" "%eax" (str new-si "(%esp)")])
         (assoc :si new-si)))))

(declare compile-expr)

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

(defn compile-immediate [x]
  (thread-state
   (asm-comment "Immediate" x)
   (instr ["movl" (str "$" (immediate-rep x)) "%eax" ])))

;;;;;;;; Primitive assembly dispatch

(defn dispatch-primcall [x] (if (seq x) (first x) :default))

(defmulti compile-primcall dispatch-primcall)

(defmethod compile-primcall :default [_] nil)

(defn primcall? [x] (not (nil? (dispatch-primcall x))))

(defmacro defprim [sym args-vec & body]
  `(defmethod compile-primcall (quote ~sym) [[_ ~@args-vec]]
     (thread-state ~@body)))

;;;;;;;; unary primitives

(defprim inc [x]
  (compile-expr x)
  (instr ["addl" (str "$" (immediate-rep 1)) "%eax"]))

(defprim dec [x]
  (compile-expr x)
  (instr ["addl" (str "$" (immediate-rep -1)) "%eax"]))

(defprim integer->char [x]
  (compile-expr x)
  (instr ["shl" "$6" "%eax"]
         ["xor" "$0x3f" "%eax"]))

(defprim char->integer [x]
  (compile-expr x)
  (instr ["shr" "$6" "%eax"]))

;;;;;;;;; Unary predicates
(defn compare-literal [lit]
  (thread-state
   (asm-comment "compare-literal" lit)
   (instr ["cmpl" lit "%eax"]            ; Set ZF to 1 if eax is our value
          ["movl" "$0" "%eax"]           ; Zero out eax
          ["sete" "%al"]                 ; Set the low byte of eax to 1 if the ZF flag is set
          ["sall" "$7" "%eax"]           ; Convert eax to a tagged boolean
          ["orl" "$63" "%eax"])))

(defn compare-immediate [x]
  (compare-literal (str "$" (immediate-rep x))))

(defprim zero? [x]
  (compile-expr x)
  (compare-immediate 0))

(defprim null? [x]
  (compile-expr x)
  (compare-immediate :empty-list))

(defprim integer? [x]
  (compile-expr x)
  (instr ["andl" (str "$" fixnum-mask) "%eax"])
  (compare-immediate fixnum-tag))

(defprim boolean? [x]
  (asm-comment "boolean?" x)
  (compile-expr x)
  (instr ["andl" (str "$" bool-mask) "%eax"])
  (compare-literal (str "$" bool-tag)))

;;;;;;;;;;;; Binary primitives
(defprim + [a b]
  (asm-comment "+" a b)
  (compile-expr a)
  (push)
  (compile-expr b)
  (fn [s] (add-instr s ["addl" (str (:si s) "(%esp)") "%eax"])))

(defprim - [a b]
  (asm-comment "-" a b)
  (compile-expr b)
  (push)
  (compile-expr a)
  (fn [s] (add-instr s ["subl" (str (:si s) "(%esp)") "%eax"])))

;; Numbers are encoded, which makes this challenging. When encoded,
;; straight enc(a) * enc(b) = enc(4 * a * b)
;; So, we use b/4 to compensate
(defprim * [a b]
  (asm-comment "*" a b)
  (compile-expr a)
  (push)
  (compile-expr b)
  (instr ["shr" "$2" "%eax"])
  (fn [s] (add-instr s ["imul" (str (:si s) "(%esp)") "%eax"])))

(defprim = [a b]
  (asm-comment "=" a b)
  (compile-expr a)
  (push)
  (compile-expr b)
  (fn [s] ((compare-literal (str (:si s) "(%esp)")) s)))

;; (defmethod compile-primcall '< [[_ a b] si]
;;   ;; ????????????????
;;   []
;; )

;;;;;;;;; Compiler
(defn compile-expr [x]
  (cond
    (immediate? x) (compile-immediate x)
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
  ["ret"
   ".cfi_endproc"])

(defn compile [exp]
  (let [start-state empty-state
        end-state ((compile-expr exp) start-state)]
    (concat prefix
            (map asm-vec-to-line (:code end-state))
            suffix)))

(defn assemble [asm]
  (spit "code.s" (str/join "\n" asm))
  (println (sh "cc" "code.s" "runner.c" "-o" "a.out" "-m32")))

(defn compile-and-run [exp]
  (-> exp compile assemble)
  (-> (sh "./a.out") :out (str/trim-newline)))

#_(compile-and-run '(integer? 42))
