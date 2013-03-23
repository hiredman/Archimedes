(ns Archimedes.bar
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [Archimedes.db :refer [types- type-db operations operations- natural-division-result]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates

(defn binaryo [op]
  (fresh
   [a]
   (operations- op 2 a)))

(defn unaryo [op]
  (fresh
   [a]
   (operations- op 1 a)))

(defn not-boxedo [type]
  (fresh
   [a b c d e]
   (types- type a b c d :unboxed e)))

(defn boxedo [type]
  (fresh
   [a b c d e]
   (types- type a b c d :boxed e)))

(defn floating-pointo [type]
  (fresh
   [a b c d e]
   (types- type a b c :floating d e)))

(defn not-floating-pointo [type]
  (fresh
   [a b c d e]
   (types- type a b c :none d e)))

(defn naturalo [type]
  (fresh
   [a b c d e]
   (types- type a :natural b c d e)))

(defn not-naturalo [type]
  (fresh
   [a b c d e]
   (types- type a :other b c d e)))

(defn fixed-widtho [type]
  (fresh
   [a b c d e]
   (types- type a b :fixed c d e)))

(defn not-fixed-widtho [type]
  (fresh
   [a b c d e]
   (types- type a b :open c d e)))

(defn returnable [type]
  (fresh
   [a b c d e]
   (types- type a b c d e :return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(defn eithero [pred arg1 arg2]
  (conda
   [(pred arg1)]
   [(pred arg2)]))

(defn botho [pred arg1 arg2]
  (fresh
   []
   (pred arg1)
   (pred arg2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(defn sameo [op arg1 arg2 return]
  (conde
   [(!= op :divide)
    (conde
     [(== arg1 arg2) (== arg1 :int) (== return :long)]
     [(== arg1 arg2) (== arg1 :float) (== return :double)]
     [(== arg1 arg2) (== arg1 :BigInteger) (== return :BigInt)]
     [(== arg1 arg2) (boxedo arg1) (not-boxedo return)]
     [(== arg1 arg2) (not-boxedo arg1) (== arg1 return)]
     [(!= arg1 arg2)])]
   [(== op :divide)]))

(defn floating-point-contaminato [arg1 arg2 return]
  (conde
   [(eithero floating-pointo arg1 arg2) (floating-pointo return)]
   [(botho not-floating-pointo arg1 arg2) (not-floating-pointo return)]))

(defn natural-division-resulto [t]
  (== natural-division-result t))

(defn not-natural-division-resulto [t]
  (!= natural-division-result t))

(defn ratio-contaminato [arg1 arg2 return]
  (conde
   [(botho not-floating-pointo arg1 arg2) (eithero natural-division-resulto arg1 arg2)
    (natural-division-resulto return)]
   [(eithero floating-pointo arg1 arg2) (eithero natural-division-resulto arg1 arg2)]
   [(not-natural-division-resulto arg1) (not-natural-division-resulto arg2)]))

;; maybe these can be combined
(defn make-contaminato [t r]
  (fn [arg1 arg2 return]
    (conde
     [(eithero #(== % t) arg1 arg2) (== return r)]
     [(!= arg1 t) (!= arg2 t)])))

(def object-contaminato (make-contaminato :Object :Number))

(def number-contaminato (make-contaminato :Number :Number))

(def bigdecimal-contaminato (make-contaminato :BigDecimal :BigDecimal))

(defn special-diviso [op arg1 arg2 return]
  (conde
   [(== op :divide) (eithero floating-pointo arg1 arg2) (floating-pointo return)]
   [(== op :divide) (botho not-floating-pointo arg1 arg2) (natural-division-resulto return)]
   [(!= op :divide)]))

(defn constrain-returno [return]
  (fresh
   []
   (returnable return)
   (not-boxedo return)))

(defn setupo [q op arg1 arg2 return]
  (fresh
   []
   (== q [op arg1 arg2 return])
   (membero op (keys operations))
   (membero arg1 (keys type-db))
   (membero arg2 (keys type-db))
   (membero return (keys type-db))))

(defn fixed-width-cantaminato [op arg1 arg2 return]
  (conde
   [(!= :divide op) (botho fixed-widtho arg1 arg2) (fixed-widtho return)]
   [(!= :divide op) (eithero not-fixed-widtho arg1 arg2) (not-fixed-widtho return)]
   [(== :divide op) (botho fixed-widtho arg1 arg2)
    (conde
     [(eithero floating-pointo arg1 arg2) (fixed-widtho return)]
     [(botho not-floating-pointo arg1 arg2) (not-fixed-widtho return)])]
   [(== :divide op) (eithero not-fixed-widtho arg1 arg2) (not-fixed-widtho return)]))

(defn natural-contaminato [op arg1 arg2 return]
  (conde
   [(!= :divide op) (botho naturalo arg1 arg2) (naturalo return)]
   [(!= :divide op) (eithero not-naturalo arg1 arg2) (not-naturalo return)]
   [(== :divide op)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; driver

(defn f []
  (run*
   [q]
   (fresh
    [op arg1 arg2 return]
    (setupo q op arg1 arg2 return)
    (binaryo op)
    (constrain-returno return)
    (sameo op arg1 arg2 return)
    (floating-point-contaminato arg1 arg2 return)
    (ratio-contaminato arg1 arg2 return)
    (object-contaminato arg1 arg2 return)
    (number-contaminato arg1 arg2 return)
    (bigdecimal-contaminato arg1 arg2 return)
    (special-diviso op arg1 arg2 return)
    (fixed-width-cantaminato op arg1 arg2 return)
    (natural-contaminato op arg1 arg2 return)
    )))

;; code gen
(defn h []
  (for [[op arg1 arg2 return :as m] (sort-by (fn [[op a1 a2 r]]
                                               [op r a1 a2]) (f))]
    (format "%s %s(%s a1, %s a2) {\n%s}"
            (name return)
            (name op)
            (name arg1)
            (name arg2)
            (if (and (= :primitive (get-in type-db [arg1 :representation]))
                     (= :primitive (get-in type-db [arg2 :representation]))
                     (= :primitive (get-in type-db [return :representation])))
              (format "  return ((%s)a1 %s (%s)a2);\n"
                      (name return)
                      (get {:subtract "-"
                            :add "+"
                            :divide "/"
                            :multiply "*"} op)
                      (name return))
              (cond
               (= :boxed (get-in type-db [arg1 :boxed-or-unboxed]))
               (format "  return %s(a1.%s, a2);\n"
                       (name op)
                       (let [method-name (str (.toLowerCase (name arg1)) "Value()")]
                         (cond
                          (= method-name "integerValue()")
                          "longValue()"
                          (= method-name "floatValue()")
                          "doubleValue()"
                          :else
                          method-name)))
               (= :boxed (get-in type-db [arg2 :boxed-or-unboxed]))
               (format "  return %s(a1, a2.%s);\n"
                       (name op)
                       (let [method-name (str (.toLowerCase (name arg2)) "Value()")]
                         (cond
                          (= method-name "integerValue()")
                          "longValue()"
                          (= method-name "floatValue()")
                          "doubleValue()"
                          :else
                          method-name)))
               (= arg1 :Number)
               (apply str
                      (concat
                       (for [[type-name {:keys [representation]}] type-db
                             :when (= :object representation)
                             :when (not= type-name :Object)
                             :when (not= type-name :Number)]
                         (format "  if (a1 instanceof %s)\n    return %s((%s)a1, (%s)a2);\n"
                                 (name type-name)
                                 (name op)
                                 (name type-name)
                                 (name arg2)))
                       ["  throw new RuntimeException(\"Unknown Number Type\");\n"]))
               (= arg2 :Number)
               (apply str
                      (concat
                       (for [[type-name {:keys [representation]}] type-db
                             :when (= :object representation)
                             :when (not= type-name :Object)
                             :when (not= type-name :Number)]
                         (format "  if (a2 instanceof %s)\n    return %s((%s)a1, (%s)a2);\n"
                                 (name type-name)
                                 (name op)
                                 (name arg1)
                                 (name type-name)))
                       ["  throw new RuntimeException(\"Unknown Number Type\");\n"]))
               (= arg1 :Object)
               (format "  return %s((Number)a1, a2);\n" (name op))
               (= arg2 :Object)
               (format "  return %s(a1, (Number)a2);\n" (name op))
               (= :BigDecimal return arg1 arg2)
               (format "  return a1.%s(a2);\n" (name op))
               (and (= :BigInteger arg1 arg2)
                    (= :BigInt return))
               (format "  return BigInt.fromBigInteger(a1.%s(a2));\n" (name op))
               (and (= arg1 :BigDecimal)
                    (= arg2 :double))
               (format "  return %s(a1, BigDecimal(a2));\n" (name op))
               (and (= arg2 :BigDecimal)
                    (= arg1 :double))
               (format "  return %s(BigDecimal(a1), a2);\n" (name op))

               :else
               (str
                "  return "
                (get {:Ratio "new Ratio(new BigInteger(\"1\"),new BigInteger(\"1\"))"
                      :BigInt "BigInt.fromLong(0)"
                      :BigDecimal "new BigDecimal(0)"}
                     return
                     "0")
                ";\n"))))))
