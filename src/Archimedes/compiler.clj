(ns Archimedes.compiler
  (:refer-clojure :exclude [compile])
  (:use [clojure.java.io :only [copy file]]
        [clojure.contrib.monads :only [state-m fetch-state]]
        [clojure.contrib.logging :only [info]])
  (:import [clojure.asm ClassWriter Type]))

(defmacro inner-with-monad [init & more]
  (if (seq more)
    (let [[binder name next & xs] more
          [binder name next xs] (if (= :as binder)
                                  [binder name next xs]
                                  [:as '_ binder (reduce
                                                  (fn [xs x]
                                                    (if x (conj xs x) xs))
                                                  xs
                                                  [next name])])]
      `(~'bind ~init
               (fn [~name]
                 (inner-with-monad ~next ~@xs))))
    init))

(defmacro in [monad & more]
  `(let [name#      ~monad
         ~'bind   (:m-bind name#)
         ~'return (:m-result name#)
         ~'m-zero   (:m-zero name#)
         ~'m-plus   (:m-plus name#)]
     (inner-with-monad ~@more)))

(defn update-state [f & args]
  (fn [state]
    [state (apply f state args)]))

;;TODO: rewrite Marchine to live inside state-m
(defprotocol Machine
  (init [machine values])
  (fin [machine values])
  (produce [machine args] "get the final product of the machine")
  (type-of [machine name] "abstract clojure type => machine type")
  (start-procedure [machine name attrs] "start a primitive proc")
  (end-procedure [machine] "end primitive proc")
  (procedure-call [machine name args] "call primitive proc")
  (function-call [machine args] "call closure object")
  (resolve-var [machine var] "resolve clojure var")
  (access-local [machine local])
  (immediate [machine value attrs])
  (start-function [machine attrs])
  (end-function [machine attrs])
  (start-namespace [machine namespace])
  (refer-to [machine namespace])
  (define [machine name value])
  (local? [machine name]))

(defprotocol Compilable
  (compile [expr machine]))

(extend-protocol
    Compilable

  clojure.lang.ISeq
  (compile [sexp machine]
    (info (pr-str sexp))
    (let [op (first sexp)
          args (rest sexp)]
      (cond
       (= 'in-ns op)
       (start-namespace machine (second (first args)))
       (= 'refer op)
       (refer-to machine (second (first args)))
       (= 'def op)
       (define machine (first args) (second args))
       (= 'fn* op)
       (let [[[arg-list & body]] args]
         (in state-m
           (start-function machine arg-list)
           (compile (cons 'do body) machine)
           (fetch-state) :as stack
           (return (println stack))
           (end-function machine nil)))
       (= 'do op)
       (if (< (count args) 2)
         (compile (first args) machine)
         (println "FOO"))
       :else
       (in state-m
         (update-state conj :fn-call)
         (reduce
          (fn [m args]
            (in state-m
              m
              (compile args machine)))
          (compile op machine)
          args)
         (function-call machine (count args))))))

  Number
  (compile [number machine]
    (immediate machine number nil))

  java.lang.String
  (compile [string machine]
    (immediate machine string nil))

  clojure.lang.Symbol
  (compile [symbol machine]
    (in state-m
      (local? machine symbol) :as local?
      (if local?
        (access-local machine symbol)
        (resolve-var machine symbol)))))

