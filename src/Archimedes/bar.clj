(ns Archimedes.bar
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

(def ^:dynamic gensym-id (atom 0))

(defn gensym [n]
  (let [id (swap! gensym-id inc)]
    (symbol (str (name n) id))))

(defprotocol HasMethods
  (-invoke [thing method args]))

(defprotocol HasFields
  (field [thing field]))

(defprotocol CanBeCast
  (C [thing type]))

(defprotocol Assignable
  (Bind [thing value]))

(defn invoke [thing m & args]
  (-invoke thing m args))

(defrecord V [type name]
  HasMethods
  (-invoke [_ method args]
    (->V nil (str name "." (clojure.core/name method) "(" (apply str (interpose \, (map :name args))) ")")))
  HasFields
  (field [_ field]
    (->V nil (str name "." (clojure.core/name field))))
  CanBeCast
  (C [_ type]
    (assert (keyword? type))
    (->V type (str "(" (clojure.core/name type)  ")" name)))
  Assignable
  (Bind [_ value]
    (assert type)
    (str (clojure.core/name type)
         " "
         (clojure.core/name name)
         " = "
         value
         ";"))
  Object
  (toString [_]
    (clojure.core/name name)))

(def ONE (->V :long "1"))

(defn ctor [type & args]
  {:pre [(keyword? type)]}
  (let [n (apply str (interpose \, (map :name args)))]
    (->V type
         (str "new " (name type) "(" n ")"))))

(defn static [method & args]
  (let [n (apply str (interpose \, args))]
    (->V nil (str (name method) "(" n ")"))))

(defn big-integer []
  (->V :BigInteger (gensym 'bi)))

(defn big-decimal []
  (->V :BigDecimal (gensym 'bd)))

(defn ratio []
  (->V :Ratio (gensym 'rat)))

(defn casto [start end in-code out-code]
  (conde
   ;; to BigDec
   [(== (:type start) :BigInteger)
    (== (:type end) :BigDecimal)
    (conso (Bind end (ctor :BigDecimal start)) in-code out-code)]
   [(== (:type start) :Ratio)
    (== (:type end) :BigDecimal)
    (conso (Bind end (invoke start :decimalValue)) in-code out-code)]
   [(== (:type start) :double)
    (== (:type end) :BigDecimal)
    (conso (Bind end (ctor :BigDecimal start)) in-code out-code)]
   [(== (:type start) :Double)
    (== (:type end) :BigDecimal)
    (conso (Bind end (ctor :BigDecimal (invoke start :doubleValue)))
           in-code out-code)]
   [(== (:type start) :float)
    (== (:type end) :BigDecimal)
    (conso (Bind end (ctor :BigDecimal (C start :double))) in-code out-code)]
   [(== (:type start) :long)
    (== (:type end) :BigDecimal)
    (let [en (->V :BigInteger (gensym 'c))]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   [(== (:type start) :int)
    (== (:type end) :BigDecimal)
    (let [en (->V :BigInteger (gensym 'c))]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   [(== (:type start) :BigInt)
    (== (:type end) :BigDecimal)
    (let [en (big-integer)]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   ;;
   ;;
   ;; BigInt
   [(== (:type start) :long)
    (== (:type end) :BigInt)
    (conso (Bind end (static :BigInt.fromLong start)) in-code out-code)]
   [(== (:type start) :int)
    (== (:type end) :BigInt)
    (conso (Bind end (static :BigInt.fromLong (C start :long))) in-code out-code)]
   [(== (:type start) :BigInteger)
    (== (:type end) :BigInt)
    (conso (Bind end (static :BigInt.fromBigInteger start)) in-code out-code)]
   ;;
   ;;
   ;;BigInteger
   [(== (:type start) :BigInt)
    (== (:type end) :BigInteger)
    (conso (Bind end (invoke start :toBigInteger)) in-code out-code)]
   [(== (:type start) :long)
    (== (:type end) :BigInteger)
    (conso (Bind end (static :BigInteger.valueOf start)) in-code out-code)]
   [(== (:type start) :int)
    (== (:type end) :BigInteger)
    (conso (Bind end (static :BigInteger.valueOf (C start :long))) in-code out-code)]
   ;;
   ;;
   ;; Ratio
   [(== (:type start) :BigInteger)
    (== (:type end) :Ratio)
    (conso (Bind end (ctor :Ratio start (static :BigInteger.valueOf ONE))) in-code out-code)]
   [(== (:type start) :BigInt)
    (== (:type end) :Ratio)
    (let [en (->V :BigInteger (gensym 'c))]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   [(== (:type start) :long)
    (== (:type end) :Ratio)
    (let [en (->V :BigInteger (gensym 'm))]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   [(== (:type start) :int)
    (== (:type end) :Ratio)
    (let [en (->V :BigInteger (gensym 'm))]
      (fresh [x]
        (casto start en in-code x)
        (casto en end x out-code)))]
   ;;
   ;;
   ;; long
   [(== (:type start) :int)
    (== (:type end) :long)
    (conso (Bind end (C start :long)) in-code out-code)]
   ;; continue
   [(== (:type start) (:type end))
    (conso (Bind end start) in-code out-code)]
   [(== (:type start) :Long)
    (== (:type end) :long)
    (conso (Bind end (invoke start :longValue)) in-code out-code)]
   [(== (:type start) :Integer)
    (== (:type end) :long)
    (conso (Bind end (invoke start :longValue)) in-code out-code)]))

(defn unbox [arg1 new-arg1 in-code out-code]
  (conde
   [(== (:type arg1) :Long)
    (== (:type new-arg1) :long)
    ;; god damn logic vars
    (conso (Bind (assoc new-arg1 :type :long) (invoke arg1 :longValue)) in-code out-code)]
   [(== (:type arg1) :Integer)
    (== (:type new-arg1) :long)
    (conso (Bind (assoc new-arg1 :type :long) (invoke arg1 :longValue)) in-code out-code)]
   [(== (:type arg1) :Double)
    (== (:type new-arg1) :double)
    (conso (Bind (assoc new-arg1 :type :double) (invoke arg1 :doubleValue)) in-code out-code)]
   [(== (:type arg1) :Float)
    (== (:type new-arg1) :double)
    (conso (Bind (assoc new-arg1 :type :double) (invoke arg1 :doubleValue)) in-code out-code)]))

(defn to-java* [free form]
  (cond
   (and (coll? form)
        (symbol? (first form))
        (.startsWith (name (first form)) ".-"))
   (fn [env]
     (let [field (name (first form))
           field (subs field 2)]
       (str ((to-java* free (second form)) env) "." field)))
   (and (coll? form)
        (symbol? (first form))
        (.startsWith (name (first form)) "."))
   (fn [env]
     (let [field (name (first form))
           field (subs field 1)]
       (str ((to-java* free (second form)) env) "." field "("
            (apply str (interpose \, (map (fn [f] (f env)) (map (partial to-java* free) (rest (rest form))))))
            ")")))
   (contains? free form)
   (fn [env]
     (get env form))
   (coll? form)
   (fn [env]
     (str (name (first form))
          "("
          (apply str (interpose \, (map (fn [f] (f env)) (map (partial to-java* free) (rest form)))))
          ")"))
   :else
   (fn [env] form)))

(def ratio-add-nom (to-java* #{'ry 'rx} '(.add (.multiply (.-numerator ry) (.-denominator rx))
                                               (.multiply (.-numerator rx) (.-denominator ry)))))

(def ratio-add-denom (to-java* #{'ry 'rx} '(.multiply (.-denominator ry)
                                                      (.-denominator rx))))

(def ratio-divide (to-java* #{'rx 'ry} '(divide rx ry)))
(def ratio-divide* (to-java* #{'rx 'ry} '(divide
                                          (.multiply (.-numerator rx)
                                                     (.-denominator ry))
                                          (.multiply (.-numerator ry)
                                                     (.-denominator rx)))))
(def ratio-multiply
  (to-java* '#{rx ry} '(divide (.multiply (.-numerator ry)
                                          (.-numerator rx))
                               (.multiply (.-denominator ry)
                                          (.-denominator rx)))))

(declare g)


(defn addo [op arg1 arg2 return in-code out-code]
  (conde
   [(== op :add)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :long)
    (fresh [a b c]
      (conso (str "long ret = " (C arg1 :long) " + " (C arg2 :long) ";") in-code a)
      (conso (str "if ((ret ^ " (:name arg1) ") < 0 && (ret ^ " arg2 ") < 0)") a b)
      (conso (str "  throw new RuntimeException(\"overflow\");") b c)
      (conso "ret;" c out-code))]
   [(== op :add)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str (C arg1 :double) " + " (C arg2 :double) ";") in-code out-code)]
   [(== return :BigDecimal)
    (== op :add)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (invoke a1 :add b1) ";") bb out-code)))]
   [(== return :BigInt)
    (== op :add)
    (let [a1 (->V :BigInt (gensym 'a))
          b1 (->V :BigInt (gensym 'b))]
      (fresh [aa bb]
        (casto  arg1 a1 in-code aa)
        (casto  arg2 b1 aa bb)
        (conso (str (invoke a1 :add b1) ";") bb out-code)))]
   [(== return :Ratio)
    (== op :add)
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (ratio-divide {'rx (ratio-add-nom {'rx (:name a1)
                                                       'ry (:name b1)})
                                   'ry (ratio-add-denom {'rx (:name a1)
                                                         'ry (:name b1)})})
                    ";")
               bb
               out-code)))]
   ))

(defn subtracto [op arg1 arg2 return in-code out-code]
  (conde
   [(== op :subtract)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :long)
    (conso (str (C arg1 :long) " - " (C arg2 :long) ";") in-code out-code)]
   [(== op :subtract)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str (C arg1 :double) " - " (C arg2 :double) ";") in-code out-code)]
   [(== return :BigDecimal)
    (== op :subtract)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (invoke a1 :subtract b1) ";") bb out-code)))]
   [(== return :BigInt)
    (== op :subtract)
    (let [a1 (big-integer)
          b1 (big-integer)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str "BigInt.fromBigInteger(" (invoke a1 :subtract b1) ");") bb out-code)))]
   [(== op :subtract)
    (conde
     [(== (:type arg1) :Ratio)
      (!= (:type arg2) :Ratio)]
     [(== (:type arg2) :Ratio)
      (!= (:type arg1) :Ratio)])
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (subtracto op a1 b1 return bb out-code)))]
   [(== op :subtract)
    (== (:type arg1) :Ratio)
    (== (:type arg2) :Ratio)
    (conso (str "add(" arg1 ","
                (ctor :Ratio (invoke (field arg2 :numerator) :negate) (field arg2 :denominator)) ");")
           in-code
           out-code)]))

(defn multiplyo [op arg1 arg2 return in-code out-code]
  (conde
   [(== op :multiply)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :long)
    (conso (str "(long)" (:name arg1) " * (long)" (:name arg2) ";") in-code out-code)]
   [(== op :multiply)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str "(double)" (:name arg1) " * (double)" (:name arg2) ";") in-code out-code)]
   [(== op :multiply)
    (conde
     [(== (:type arg1) :Ratio)
      (!= (:type arg2) :Ratio)]
     [(== (:type arg2) :Ratio)
      (!= (:type arg1) :Ratio)])
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str "multiply(" (:name a1) ", " (:name b1) ");")
               bb
               out-code)))]
   [(== return :BigInt)
    (== op :multiply)
    (let [a1 (big-integer)
          b1 (big-integer)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str "BigInt.fromBigInteger(" (invoke a1 :multiply b1) ");") bb out-code)))]
   [(== return :BigDecimal)
    (== op :multiply)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (invoke a1 :multiply b1) ";") bb out-code)))]
   [(== op :multiply)
    (== (:type arg1) :Ratio)
    (== (:type arg2) :Ratio)
    (conso (str (ratio-multiply {'rx (:name arg1) 'ry (:name arg2)}) ";")
           in-code
           out-code)]))

(defn divideo [op arg1 arg2 return  in-code out-code]
  (conde
   [(== op :divide)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str (C arg1 :double) " / " (C arg2 :double) ";") in-code out-code)]
   [(== op :divide)
    (== return :Ratio)
    (let [a1 (big-integer)
          b1 (big-integer)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (ctor :Ratio a1 b1) ";") 
               bb
               out-code)))]
   [(== op :divide)
    (conde
     [(== (:type arg1) :Ratio)
      (!= (:type arg2) :Ratio)]
     [(== (:type arg2) :Ratio)
      (!= (:type arg1) :Ratio)])
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (divideo op a1 b1 return bb out-code)))]
   [(== op :divide)
    (== (:type arg1) :Ratio)
    (== (:type arg2) :Ratio)
    (conso (str (ratio-divide* {'rx arg1 'ry arg2}) ";")
           in-code
           out-code)]
   [(== return :BigDecimal)
    (== op :divide)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (invoke a1 :divide b1) ";") bb out-code)))]))

(defn quotiento [op arg1 arg2 return in-code out-code]
  (conde
   [(== op :quotient)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :long)
    (conso (str (C arg1 :long) " / " (C arg2 :long) ";") in-code out-code)]
   [(== op :quotient)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str (C arg1 :double) " / " (C arg2 :double) ";") in-code out-code)]
   [(== return :BigInt)
    (== op :quotient)
    (let [a1 (big-integer)
          b1 (big-integer)]
      (fresh [aa bb cc]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (static :BigInt.fromBigInteger (invoke a1 :divide b1)) ";") bb out-code)))]
   [(== return :BigDecimal)
    (== op :quotient)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str a1 ".divide(" b1 ");") bb out-code)))]
   [(== op :quotient)
    (conde
     [(== (:type arg1) :Ratio)
      (!= (:type arg2) :Ratio)]
     [(== (:type arg2) :Ratio)
      (!= (:type arg1) :Ratio)])
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (g op a1 b1 return bb out-code)))]
   [(== op :quotient)
    (== (:type arg1) :Ratio)
    (== (:type arg2) :Ratio)
    (conso (str (ratio-divide* {'rx (:name arg1) 'ry (:name arg2)}) ";")
           in-code
           out-code)]
   ))

(defn remaindero [op arg1 arg2 return in-code out-code]
  (conde
   [(== op :remainder)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :long)
    (conso (str (C arg1 :long) " / " (C arg2 :long) ";") in-code out-code)]
   [(== op :remainder)
    (not-boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (== return :double)
    (conso (str (C arg1 :double) " / " (C arg2 :double) ";") in-code out-code)]
   [(== return :BigInt)
    (== op :remainder)
    (let [a1 (big-integer)
          b1 (big-integer)]
      (fresh [aa bb cc]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (static :BigInt.fromBigInteger (invoke a1 :divide b1)) ";") bb out-code)))]
   [(== return :BigDecimal)
    (== op :remainder)
    (let [a1 (big-decimal)
          b1 (big-decimal)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (conso (str (invoke a1 :divide b1) ";") bb out-code)))]
   [(== op :remainder)
    (conde
     [(== (:type arg1) :Ratio)
      (!= (:type arg2) :Ratio)]
     [(== (:type arg2) :Ratio)
      (!= (:type arg1) :Ratio)])
    (let [a1 (ratio)
          b1 (ratio)]
      (fresh [aa bb]
        (casto arg1 a1 in-code aa)
        (casto arg2 b1 aa bb)
        (g op a1 b1 return bb out-code)))]
   [(== op :remainder)
    (== (:type arg1) :Ratio)
    (== (:type arg2) :Ratio)
    (conso (str (ratio-divide* {'rx (:name arg1) 'ry (:name arg2)}) ";")
           in-code
           out-code)]))

(defn g [op arg1 arg2 return in-code out-code]
  (conde
   [(addo op arg1 arg2 return in-code out-code)]
   [(subtracto op arg1 arg2 return in-code out-code)]
   [(multiplyo op arg1 arg2 return in-code out-code)]
   [(divideo op arg1 arg2 return in-code out-code)]
   [(quotiento op arg1 arg2 return in-code out-code)]
   [(remaindero op arg1 arg2 return in-code out-code)]
   [(boxedo (:type arg1))
    (boxedo (:type arg2))
    (let [a1 (->V (lvar) (gensym 'a))
          b1 (->V (lvar) (gensym 'b))]
      (fresh [aa bb]
        (unbox arg1 a1 in-code aa)
        (unbox arg2 b1 aa bb)
        (g op a1 b1 return bb out-code)))]
   [(boxedo (:type arg1))
    (not-boxedo (:type arg2))
    (let [a1 (->V (lvar) (gensym 'a))]
      (fresh [aa]
        (unbox arg1 a1 in-code aa)
        (g op a1 arg2 return aa out-code)))]
   [(boxedo (:type arg2))
    (not-boxedo (:type arg1))
    (let [b1 (->V (lvar) (gensym 'b))]
      (fresh [aa]
        (unbox arg2 b1 in-code aa)
        (g op arg1 b1 return aa out-code)))]))

(defn check-cast [op arg1 arg2]
  (for [[k v] type-db
        :when (= :object (:representation v))
        :when (not= k :Object)
        :when (not= k :Number)]
    (format "if (%s instanceof %s)\n   return %s(%s, (%s)%s);"
            arg1
            (name k)
            (name op)
            arg2
            (name k)
            arg1)))

;; code gen
(defn h []
  (for [[op arg1 arg2 return :as m] (sort-by (fn [[op a1 a2 r]]
                                               [op r a1 a2]) (f))
        :let [_ (binding [*out* *err*]
                  #_(prn m))
              [an1 an2 [x]] (binding [gensym-id (atom 0)]
                              (let [an1 (gensym 'a)
                                    an2 (gensym 'b)]
                                [an1 an2
                                 (doall
                                  (run 1 [q]
                                    (g op (->V arg1 an1) (->V arg2 an2) return () q)))]))
              x (reverse x)]]
    (cond
     (seq x)
     (str "public static " (name return) " " (name op) "(" (name arg1) " " an1 ", " (name arg2) " " an2 ") {\n"
          (with-out-str
            (doseq [i (butlast x)]
              (println " " i)))
          "  return " (last x)
          "\n}\n")
     (or (= arg1 :Number)
         (= arg1 :Object))
     (str "public static " (name return) " " (name op) "(" (name arg1) " " an1 ", " (name arg2) " " an2 ") {\n"
          (with-out-str
            (doseq [i (check-cast op an1 an2)]
              (println " " i))
            (println "\n  throw new RuntimeException(\"unknown number\");"))
          "}\n")
     (or (= arg2 :Number)
         (= arg2 :Object))
     (str "public static " (name return) " " (name op) "(" (name arg1) " " an1 ", " (name arg2) " " an2 ") {\n"
          (with-out-str
            (doseq [i (check-cast op an2 an1)]
              (println " " i))
            (println "\n  throw new RuntimeException(\"unknown number\");"))
          "}\n")
     :else
     (format "public static %s %s(%s a1, %s a2) FOO\n"
             (name return)
             (name op)
             (name arg1)
             (name arg2)))))


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
            (doseq [l (h)]
              (println l))
            (catch Exception e
              (.printStackTrace e)))
          (println "}"))))
