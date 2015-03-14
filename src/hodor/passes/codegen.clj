(ns hodor.passes.codegen
  (:require [hodor.passes.encode-immediates :refer [immediate?
                                                    fixnum-mask fixnum-tag
                                                    bool-mask bool-tag]]))

(defn codegen-immediate [x]
  [["movl" x "%eax" ]])

(defn dispatch-primcall [x] (if (seq x) (first x) :default))
(defmulti codegen-primcall dispatch-primcall)
(defmethod codegen-primcall :default [_] nil)
(defn primcall? [x] (not (nil? (dispatch-primcall x))))

(defmacro defprim [sym args-vec & body]
  `(defmethod codegen-primcall (quote ~sym) [[_ ~@args-vec]]
     (concat ~@body)))

(defn codegen-expr [x]
  (cond
    (immediate? x) (codegen-immediate x)
    (primcall? x) (codegen-primcall x)))

;;;;;;;; Unary primitives
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
(defn compare-literal [lit]
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

