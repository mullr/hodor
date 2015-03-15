(ns hodor.passes.codegen-stack-ops
  (:require [clojure.zip :as zip]
            [clojure.core.match :refer [match]]
            [hodor.passes.util :refer [asm-zip first-child]]))

(defn codegen-stack-ops [asm-vec]
  (loop [si 0, env {}, loc (asm-zip asm-vec)]
    (if (zip/end? loc)
      (zip/root loc)

      (match (zip/node loc)

        [:push source-reg]
        (let [new-si (- si 4)]
          (recur new-si env
                 (zip/replace loc ["movl" source-reg (str new-si "(%esp)")])))

        [:pop]
        (recur (+ si 4) env (-> loc zip/remove zip/next))

        [:stack relative-offset]
        (recur si env
               (zip/replace loc
                            (str (+ si (* 4 relative-offset)) "(%esp)")))

        [:store-stack-var source-reg sym]
        (recur si (assoc env sym (- si 4))
               (zip/replace loc [:push source-reg]))

        [:load-stack-var dest-reg sym]
        (let [offset (env sym)]
          (recur si env
                 (zip/replace loc ["movl" (str offset "(%esp)") dest-reg])))

        [:pop-stack-var sym]
        (recur si (dissoc env sym) (zip/replace loc [:pop]))

        :else (recur si env (zip/next loc))))))

