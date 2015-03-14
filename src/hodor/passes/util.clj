(ns hodor.passes.util
  (:require [clojure.zip :as zip]))

(defn asm-zip
  "Construct a zipper that traverses both vectors and seqs, producing
  seqs when editing."
  [root]
  (zip/zipper #(or (seq? %) (vector? %))
              seq
              (fn [node children] (with-meta children (meta node)))
              root))
