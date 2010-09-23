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
  (define-function [machine attrs]))

(defprotocol Compilable
  (generate-code [expr machine]))

(extend-protocol Compilable
  clojure.lang.ISeq
  (generate-code
   [expr cxt]
   (let [oper (first expr)
         args (rest expr)]
     (cond
      (= 'fn* oper)
      (define-function cxt expr)
      (= 'do oper)
      (last (doall (map #(generate-code % cxt) (rest expr))))
      (= '. oper)
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
      (and (symbol? oper)
           (:inline (meta (resolve oper)))
           ((:inline-arities (meta (resolve oper))) (count args)))
      (recur (apply (:inline (meta (resolve oper))) args) cxt)
      :else
      (let [context (dissoc (generate-code (first expr)
                                           (assoc cxt :fn-call true))
                            :fn-call)
            context (reduce #(generate-code %2 %) context args)]
        (function-call context (count args))))))
  clojure.lang.Symbol
  (generate-code
   [exp cxt]
   (if (contains? (set (:locals cxt)) exp)
     (access-local cxt exp)
     (resolve-var cxt (resolve exp))))
  java.lang.Number
  (generate-code
   [exp cxt]
   (immediate cxt exp nil))
  java.lang.Object
  (generate-code
   [exp cxt]
   (immediate cxt exp nil)))
