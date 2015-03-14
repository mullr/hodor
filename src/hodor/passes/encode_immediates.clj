(ns hodor.passes.encode-immediates
  (:require [clojure.zip :as zip]
            [hodor.passes.util :refer [asm-zip]]))

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

(defn prefixed-immediate-rep [x]
  (str "$" (immediate-rep x)))

(defn encode-immediates [asm-vec]
  (loop [loc (asm-zip asm-vec)]
    (cond
      (zip/end? loc) (zip/root loc)

      (immediate? (zip/node loc))
      (-> loc (zip/edit prefixed-immediate-rep) zip/next recur)

      :else (-> loc zip/next recur))))
