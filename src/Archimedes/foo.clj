(ns Archimedes.foo
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(def types
  #{::long
    ::double
    ::int
    ::float
    ::bigdecimal
    ::number
    ::object
    ::ratio
    ::bigint
    })

(def operations
  #{::add
    ::subtract
    ::multiply
    ::divide
    })

(def type-sizes
  {::long 2
   ::double 2
   ::int 1
   ::float 1
   ::bigdecimal 3
   ::number 3
   ::object 3
   ::ratio 3
   ::bigint 3
   })

(derive ::float ::floating-point)
(derive ::double ::floating-point)
(derive ::bigdecimal ::floating-point)

(derive ::float ::primitive)
(derive ::double ::primitive)
(derive ::long ::primitive)
(derive ::int ::primitive)

(derive ::min ::compare)
(derive ::max ::compare)

(defn not-floating-pointo [thing]
  (fn [s]
    (when-not (isa? thing ::floating-point)
      s)))

(defn compareo [thing]
  (fn [s]
    (when-not (isa? thing ::compare)
      s)))

(defn primitiveo [thing]
  (fn [s]
    (when (isa? thing ::primitive)
      s)))

(defn floating-pointo [thing]
  (fn [s]
    (when (isa? (get (.s s) thing) ::floating-point)
      s)))

(defn not-floating-pointo [thing]
  (fn [s]
    (when-not (isa? (get (.s s) thing) ::floating-point)
      s)))

(defn primitiveo [thing]
  (fn [s]
    (when (isa? (get (.s s) thing) ::primitive)
      s)))

(defn not-primitiveo [thing]
  (fn [s]
    (when-not (isa? (get (.s s) thing) ::primitive)
      s)))

(defn big-enougho
  "is the t1 type big enough for a result from the t2 type"
  [t1 t2]
  (fn [s]
    (let [t1 (get type-sizes (get (.s s) t1))
          t2 (get type-sizes (get (.s s) t2))]
      (when (>= t1 t2)
        s))))

(defn fp-contaminato [op arg1 arg2 return]
  (condu
   [(floating-pointo arg1)
    (floating-pointo return)]
   [(floating-pointo arg2)
    (floating-pointo return)]
   [(not-floating-pointo arg1)
    (not-floating-pointo arg2)
    (not-floating-pointo return)]))

(defn object-contaminato [arg1 arg2 return]
  (condu
   [(== ::object arg1) (== return ::number)]
   [(== ::object arg2) (== return ::number)]
   [(!= ::object arg1) (!= ::object arg2)]))

(defn number-contaminato [arg1 arg2 return]
  (condu
   [(== ::number arg1) (== return ::number)]
   [(== ::number arg2) (== return ::number)]
   [(== ::bigint arg1) (== return ::number)]
   [(== ::bigint arg2) (== return ::number)]
   [(!= ::number arg1) (!= ::number arg2)]))

(defn primitive-contaminato [op arg1 arg2 return]
  (condu
   [(primitiveo arg1)
    (primitiveo arg2)
    (conde
     [(primitiveo return)]
     [(== op ::divide) (== ::ratio return)]
     [(== op ::min)])]
   [(not-primitiveo arg1) (not-primitiveo return)]
   [(not-primitiveo arg2) (not-primitiveo return)]))

(defn divide-special-caseo [op arg1 arg2 return]
  (conde
   [(not-floating-pointo arg1)
    (not-floating-pointo arg2)
    (== op ::divide)
    (== return ::ratio)]
   [(== op ::divide)
    (floating-pointo arg1)
    (floating-pointo arg2)
    (floating-pointo return)]
   [(!= op ::divide)]))

(defn least-surpriseo [op arg1 arg2 return]
  (condu
   [(== arg1 arg2) (== return arg1)]
   [(== return return)]))

(defn ratio-contaminato [arg1 arg2 return]
  (conde
   [(== arg1 ::ratio)
    (not-floating-pointo arg2)
    (== return ::ratio)]
   [(== arg2 ::ratio)
    (not-floating-pointo arg1)
    (== return ::ratio)]
   [(condu
     [(!= arg1 ::ratio)
      (!= arg2 ::ratio)]
     [(floating-pointo arg1)]
     [(floating-pointo arg2)])]))

(defn f []
  (run*
   [q]
   (fresh
    [op arg1 arg2 return]
    (== q [op arg1 arg2 return])
    (membero op (seq operations))
    (membero arg1 (seq types))
    (membero arg2 (seq types))
    (membero return (seq types))
    ;; floating point contaminates
    (fp-contaminato op arg1 arg2 return)
    ;; the return type should be big enough to contain the
    ;; results of ops on the arg types
    (big-enougho return arg1)
    (big-enougho return arg2)
    ;; if any arg is a Object the result is Number
    (object-contaminato arg1 arg2 return)
    ;; if any arg is a Number the result is Number
    (number-contaminato arg1 arg2 return)
    ;; primitive args should have a primitive result
    (primitive-contaminato op arg1 arg2 return)
    ;; division returns floating point or ratios
    (divide-special-caseo op arg1 arg2 return)
    ;; math ops should always return a Number
    (!= return ::object)
    ;; return longs or doubles
    (!= return ::int)
    (!= return ::float)
    ;; if arg1 and arg2 are the same type, return should also be that type
    (least-surpriseo op arg1 arg2 return)
    ;;
    (ratio-contaminato arg1 arg2 return))))
