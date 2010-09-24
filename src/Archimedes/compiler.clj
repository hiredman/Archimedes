(ns Archimedes.compiler
  (:use [clojure.java.io :only [copy file]])
  (:import [clojure.asm ClassWriter Type]))

(defprotocol Machine
  (init [machine values])
  (fin [machine values])
  (type-of [machine name] "abstract clojure type => machine type")
  (start-procedure [machine name attrs] "start a primitive proc")
  (end-procedure [machine] "end primitive proc")
  (procedure-call [machine name args] "call primitive proc")
  (function-call [machine args] "call closure object")
  (resolve-var [machine var] "resolve clojure var")
  (access-local [machine local])
  (immediate [machine value attrs])
  (define-function [machine attrs])
  (start-namespace [machine namespace])
  (define [machine name value]))

(defprotocol Compilable
  (generate-code [expr machine]))

(extend-protocol Compilable

  clojure.lang.ISeq
  (generate-code
   [expr cxt]
   (println "Compiling:" expr)
   (let [cxt (dissoc cxt :do)
         oper (first expr)
         args (rest expr)]
     (cond
      ;;TODO: should be a multimethod
      (= 'def oper)
      (define cxt (first args) (second args))
      (= 'quote oper)
      (immediate cxt (first args) nil)
      (= 'fn* oper)
      (define-function cxt expr)
      (= 'do oper)
      (let [machine (dissoc
                     (reduce
                      #(generate-code %2 %)
                      (assoc cxt :do true)
                      (butlast args))
                     :do)]
        (generate-code (last args) machine))
      (= '. oper)
      ;;TODO: static/instance method dichotomy is a feature of the
      ;;jvm, so most of this logic should live inside the
      ;;implementation of the Machine protocol
      (let [[_ klass-or-object method & args] expr
            [method args] (if (seq? method)
                            [(first method) (rest method)]
                            [method args])]
        (if (class? (resolve klass-or-object))
          (let [cxt (reduce #(generate-code %2 %) cxt args)]
            (procedure-call cxt (keyword (name klass-or-object)
                                         (name method))
                            {:argc (count args)
                             :static true}))))
      (and (symbol? oper) ;boom, inlined!, using reflection :(
           (:inline (meta (resolve oper)))
           ((:inline-arities (meta (resolve oper))) (count args)))
      (recur (apply (:inline (meta (resolve oper))) args) cxt)
      :else
      (let [context (if (= 'in-ns oper)
                      (start-namespace cxt (last (first args)))
                      cxt)
            context (dissoc (generate-code oper
                                           (assoc context :fn-call true))
                            :fn-call)
            context (reduce #(generate-code %2 %) context args)]
        (function-call context (count args))))))

  clojure.lang.Symbol
  (generate-code
   [exp cxt]
   (println "Compiling:" exp)
   (if (contains? (set (:locals cxt)) exp)
     (access-local cxt exp)
     (resolve-var cxt
                  (or (ns-resolve ;a lot of this logic should be moved
                                  ;into the machine to I think
                       (create-ns
                        (:namespace cxt (.getName *ns*))) exp)
                      (ns-resolve 'clojure.core exp)))))

  java.lang.Number
  (generate-code
   [exp cxt]
   (immediate cxt exp nil))

  java.lang.String
  (generate-code
   [exp cxt]
   (immediate cxt exp nil)))
