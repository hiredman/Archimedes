(ns Archimedes.bar
  (:refer-clojure :exclude [== gensym])
  (:require [clojure.core.logic :refer :all]
            [Archimedes.db :refer [types- type-db operations operations- natural-division-result]]
            [Archimedes.codegen :refer [h]]
            [clojure.java.io :as io]
            [Archimedes.predicates :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(defn eithero [pred arg1 arg2]
  (conda
   [(pred arg1)]
   [(pred arg2)]))

(defn botho [pred arg1 arg2]
  (fresh []
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
  (fresh []
    (returnable return)
    (not-boxedo return)))

(defn setupo [q op arg1 arg2 return]
  (fresh []
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
  (run* [q]
    (fresh [op arg1 arg2 return]
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

(defn code-gen [output class-name]
  (.mkdirs (.getParentFile (io/file output)))
  (spit output
        (with-out-str
          (println
           "package archimedes;

import java.math.BigDecimal;
import java.math.BigInteger;
import clojure.lang.Ratio;
import clojure.lang.BigInt;

    public class" class-name "{")
          (try
            (doseq [l (h f)]
              (println l))
            (catch Exception e
              (.printStackTrace e)))
          (println "}"))))
