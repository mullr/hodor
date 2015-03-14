(ns hodor.passes.codegen-stack-ops
  (:require [clojure.zip :as zip]
            [hodor.passes.util :refer [asm-zip]]))

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
