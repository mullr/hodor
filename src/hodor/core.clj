(ns hodor.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [hodor.passes.uniqueify-lets :refer [uniqueify-lets]]
            [hodor.passes.codegen :refer [codegen-expr]]
            [hodor.passes.codegen-stack-ops :refer [codegen-stack-ops]]
            [hodor.passes.encode-immediates :refer [encode-immediates]]))

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

(defn compile-to-asm [expr]
  (let [asm (-> expr
                uniqueify-lets
                codegen-expr
                codegen-stack-ops
                encode-immediates)]
    (concat prefix
            (map asm-vec-to-line asm)
            suffix)))

(defn assemble [asm out-file]
  (spit "code.s" (str/join "\n" asm))
  (println (sh "cc" "code.s" "runner.c" "-o" out-file "-m32")))

(defn compile-and-run [exp]
  (-> exp compile-to-asm (assemble "a.out"))
  (-> (sh "./a.out") :out (str/trim-newline)))

