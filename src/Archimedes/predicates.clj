(ns Archimedes.predicates
  (:refer-clojure :exclude [== gensym])
  (:require [clojure.core.logic :refer :all]
            [Archimedes.db :refer [types- type-db operations operations- natural-division-result]]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates

(defn binaryo [op]
  (fresh [a]
    (operations- op 2 a)))

(defn unaryo [op]
  (fresh [a]
    (operations- op 1 a)))

(defn not-boxedo [type]
  (fresh [a b c d e]
    (types- type a b c d :unboxed e)))

(defn boxedo [type]
  (fresh [a b c d e]
    (types- type a b c d :boxed e)))

(defn floating-pointo [type]
  (fresh [a b c d e]
    (types- type a b c :floating d e)))

(defn not-floating-pointo [type]
  (fresh [a b c d e]
    (types- type a b c :none d e)))

(defn naturalo [type]
  (fresh [a b c d e]
    (types- type a :natural b c d e)))

(defn not-naturalo [type]
  (fresh [a b c d e]
    (types- type a :other b c d e)))

(defn fixed-widtho [type]
  (fresh [a b c d e]
    (types- type a b :fixed c d e)))

(defn not-fixed-widtho [type]
  (fresh [a b c d e]
    (types- type a b :open c d e)))

(defn returnable [type]
  (fresh [a b c d e]
    (types- type a b c d e :return)))
