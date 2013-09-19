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

(defn casto [start-type end-type start-name end-name in-code out-code]
  (conde
   ;; to BigDec
   [(== start-type :BigInteger)
    (== end-type :BigDecimal)
    (conso (str (name end-type) " " end-name " = new BigDecimal(" start-name ");") in-code out-code)]
   ;;
   ;; TODO: not right
   [(== start-type :Ratio)
    (== end-type :BigDecimal)
    (conso (str (name end-type) " " end-name " = " start-name ".decimalValue();") in-code out-code)]
   [(== start-type :double)
    (== end-type :BigDecimal)
    (conso (str (name end-type) " " end-name " = new BigDecimal(" start-name ");") in-code out-code)]
   [(== start-type :float)
    (== end-type :BigDecimal)
    (conso (str (name end-type) " " end-name " = new BigDecimal((double)" start-name ");") in-code out-code)]
   [(== start-type :long)
    (== end-type :BigDecimal)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto :long :BigInteger start-name en in-code x)
        (casto :BigInteger :BigDecimal en end-name x out-code)))]
   [(== start-type :int)
    (== end-type :BigDecimal)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto start-type :BigInteger start-name en in-code x)
        (casto :BigInteger end-type en end-name x out-code)))]
   [(== start-type :BigInt)
    (== end-type :BigDecimal)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto :BigInt :BigInteger start-name en in-code x)
        (casto :BigInteger :BigDecimal en end-name x out-code)))]
   ;;
   ;;
   ;;
   [(== start-type :BigInt)
    (== end-type :BigInteger)
    (conso (str (name end-type) " " end-name " = " start-name ".toBigInteger();") in-code out-code)]
   [(== start-type :long)
    (== end-type :BigInt)
    (conso (str (name end-type) " " end-name " = BigInt.fromLong(" start-name ");") in-code out-code)]
   [(== start-type :int)
    (== end-type :BigInt)
    (conso (str (name end-type) " " end-name " = BigInt.fromLong((int)" start-name ");") in-code out-code)]
   [(== start-type :long)
    (== end-type :BigInteger)
    (conso (str (name end-type) " " end-name " = BigInteger.valueOf(" start-name ");") in-code out-code)]
   [(== start-type :int)
    (== end-type :BigInteger)
    (conso (str (name end-type) " " end-name " = BigInteger.valueOf((long)" start-name ");") in-code out-code)]
   [(== start-type :int)
    (== end-type :long)
    (conso (str (name end-type) " " end-name " = (long)" start-name ";") in-code out-code)]
   [(== start-type :BigInteger)
    (== end-type :BigInt)
    (conso (str (name end-type) " " end-name " = BigInt.fromBigInteger(" start-name ");") in-code out-code)]
   [(== start-type :BigInteger)
    (== end-type :Ratio)
    (conso (str (name end-type) " " end-name " = new Ratio(" start-name ", BigInteger.valueOf(1));") in-code out-code)]
   [(== start-type :BigInt)
    (== end-type :Ratio)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto :BigInt :BigInteger start-name en in-code x)
        (casto :BigInteger :Ratio en end-name x out-code)))]
   [(== start-type :long)
    (== end-type :Ratio)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto :long :BigInteger start-name en in-code x)
        (casto :BigInteger :Ratio en end-name x out-code)))]
   [(== start-type :int)
    (== end-type :Ratio)
    (let [en (gensym 'm)]
      (fresh [x]
        (casto start-type :BigInteger start-name en in-code x)
        (casto :BigInteger end-type en end-name x out-code)))]
   ;; continue
   [(== start-type end-type)
    (conso (str (name end-type) " " end-name " = " start-name ";") in-code out-code)]
   [(== start-type :Long)
    (== end-type :long)
    (conso (str "long " end-name " = " start-name ".longValue();") in-code out-code)]))

(defn unbox [arg1-type arg1 new-arg1-type new-arg1 in-code out-code]
  (conde
   [(== arg1-type :Long)
    (== new-arg1-type :long)
    (conso (str "long " new-arg1 " = " arg1 ".longValue();") in-code out-code)]
   [(== arg1-type :Integer)
    (== new-arg1-type :long)
    (conso (str "long " new-arg1 " = " arg1 ".longValue();") in-code out-code)]
   [(== arg1-type :Double)
    (== new-arg1-type :double)
    (conso (str "double " new-arg1 " = " arg1 ".doubleValue();") in-code out-code)]
   [(== arg1-type :Float)
    (== new-arg1-type :double)
    (conso (str "double " new-arg1 " = " arg1 ".doubleValue();") in-code out-code)]))

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

(defn g [op arg1-type arg2-type return arg1 arg2 in-code out-code]
  (conde
   [(== op :add)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :long)
    (fresh [a b c]
      (conso (str "long ret = (long)" arg1 " + (long)" arg2 ";") in-code a)
      (conso (str "if ((ret ^ " arg1 ") < 0 && (ret ^ " arg2 ") < 0)") a b)
      (conso (str "  throw new RuntimeException(\"overflow\");") b c)
      (conso "ret;" c out-code))]
   [(== op :subtract)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :long)
    (conso (str "(long)" arg1 " - (long)" arg2 ";") in-code out-code)]
   [(== op :multiply)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :long)
    (conso (str "(long)" arg1 " * (long)" arg2 ";") in-code out-code)]
   [(== op :divide)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :double)
    (conso (str "(double)" arg1 " / (double)" arg2 ";") in-code out-code)]
   [(== op :multiply)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :double)
    (conso (str "(double)" arg1 " * (double)" arg2 ";") in-code out-code)]
   [(== op :add)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :double)
    (conso (str "(double)" arg1 " + (double)" arg2 ";") in-code out-code)]
   [(== op :subtract)
    (not-boxedo arg1-type)
    (not-boxedo arg2-type)
    (== return :double)
    (conso (str "(double)" arg1 " - (double)" arg2 ";") in-code out-code)]
   [(boxedo arg1-type)
    (boxedo arg2-type)
    (let [new-arg1 (gensym 'a)
          new-arg2 (gensym 'b)]
      (fresh [new-arg1-type
              new-arg2-type
              a b]
        (unbox arg1-type arg1 new-arg1-type new-arg1 in-code a)
        (unbox arg2-type arg2 new-arg2-type new-arg2 a b)
        (g op new-arg1-type new-arg2-type return new-arg1 new-arg2 b out-code)))]
   [(not-boxedo arg1-type)
    (boxedo arg2-type)
    (let [new-arg2 (gensym 'b)]
      (fresh [new-arg2-type b]
        (unbox arg2-type arg2 new-arg2-type new-arg2 in-code b)
        (g op arg1-type new-arg2-type return arg1 new-arg2 b out-code)))]
   [(boxedo arg1-type)
    (not-boxedo arg2-type)
    (let [new-arg1 (gensym 'a)]
      (fresh [new-arg1-type a]
        (unbox arg1-type arg1 new-arg1-type new-arg1 in-code a)
        (g op new-arg1-type arg2-type return new-arg1 arg2 a out-code)))]
   ;; [(!= arg1-type arg2-type)]
   [(== return :BigDecimal)
    (== op :add)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigDecimal arg1 a1 in-code aa)
        (casto arg2-type :BigDecimal arg2 b1 aa bb)
        (conso (str a1 ".add(" b1 ");") bb out-code)))]
   [(== return :BigInt)
    (== op :add)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigInt arg1 a1 in-code aa)
        (casto arg2-type :BigInt arg2 b1 aa bb)
        (conso (str a1 ".add(" b1 ");") bb out-code)))]
   [(== return :Ratio)
    (== op :add)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :Ratio arg1 a1 in-code aa)
        (casto arg2-type :Ratio arg2 b1 aa bb)
        (conso (str (ratio-divide {'rx (ratio-add-nom {'rx a1 'ry b1})
                                   'ry (ratio-add-denom {'rx a1 'ry b1})})
                    ";")
               bb
               out-code)))]
   ;; [(== arg1-type :int)
   ;;  (let [a1 (gensym 'a)]
   ;;    (fresh [aa]
   ;;      (casto arg1-type :long arg1 a1 in-code aa)
   ;;      (g op :long arg2-type return a1 arg2 aa out-code)))]
   ;; [(== arg2-type :int)
   ;;  (let [b1 (gensym 'a)]
   ;;    (fresh [bb]
   ;;      (casto arg2-type :long arg2 b1 in-code bb)
   ;;      (g op arg1-type :long return arg1 b1 bb out-code)))]
   [(== op :divide)
    (== return :Ratio)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigInteger arg1 a1 in-code aa)
        (casto arg2-type :BigInteger arg2 b1 aa bb)
        (conso (str "new Ratio(" a1 ", " b1 ");")
               bb
               out-code)))]
   [(== op :divide)
    (conde
     [(== arg1-type :Ratio)
      (!= arg2-type :Ratio)]
     [(== arg2-type :Ratio)
      (!= arg1-type :Ratio)])
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :Ratio arg1 a1 in-code aa)
        (casto arg2-type :Ratio arg2 b1 aa bb)
        (conso (str "divide(" a1 ", " b1 ");")
               bb
               out-code)))]
   [(== op :divide)
    (== arg1-type :Ratio)
    (== arg2-type :Ratio)
    (conso (to-java* {'rx arg1 'ry arg2} ratio-divide*)
           in-code
           out-code)]
   [(== return :BigDecimal)
    (== op :divide)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigDecimal arg1 a1 in-code aa)
        (casto arg2-type :BigDecimal arg2 b1 aa bb)
        (conso (str a1 ".divide(" b1 ");") bb out-code)))]
   [(== return :BigDecimal)
    (== op :subtract)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigDecimal arg1 a1 in-code aa)
        (casto arg2-type :BigDecimal arg2 b1 aa bb)
        (conso (str a1 ".subtract(" b1 ");") bb out-code)))]
   [(== return :BigInt)
    (== op :subtract)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigInteger arg1 a1 in-code aa)
        (casto arg2-type :BigInteger arg2 b1 aa bb)
        (conso (str a1 ".subtract(" b1 ");") bb out-code)))]
   [(== op :subtract)
    (conde
     [(== arg1-type :Ratio)
      (!= arg2-type :Ratio)]
     [(== arg2-type :Ratio)
      (!= arg1-type :Ratio)])
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :Ratio arg1 a1 in-code aa)
        (casto arg2-type :Ratio arg2 b1 aa bb)
        (conso (str "subtract(" a1 ", " b1 ");")
               bb
               out-code)))]
   [(== op :subtract)
    (== arg1-type :Ratio)
    (== arg2-type :Ratio)
    (conso (str "add(new Ratio(" arg1 "," arg2 ".numerator.negate()," arg2 ".denominator));")
           in-code
           out-code)]
   [(== op :multiply)
    (conde
     [(== arg1-type :Ratio)
      (!= arg2-type :Ratio)]
     [(== arg2-type :Ratio)
      (!= arg1-type :Ratio)])
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :Ratio arg1 a1 in-code aa)
        (casto arg2-type :Ratio arg2 b1 aa bb)
        (conso (str "multiply(" a1 ", " b1 ");")
               bb
               out-code)))]
   [(== return :BigInt)
    (== op :multiply)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigInteger arg1 a1 in-code aa)
        (casto arg2-type :BigInteger arg2 b1 aa bb)
        (conso (str a1 ".multiply(" b1 ");") bb out-code)))]
   [(== return :BigDecimal)
    (== op :multiply)
    (let [a1 (gensym 'a)
          b1 (gensym 'b)]
      (fresh [aa bb]
        (casto arg1-type :BigDecimal arg1 a1 in-code aa)
        (casto arg2-type :BigDecimal arg2 b1 aa bb)
        (conso (str a1 ".multiply(" b1 ");") bb out-code)))]))

;; code gen
(defn h []
  (for [[op arg1 arg2 return :as m] (sort-by (fn [[op a1 a2 r]]
                                               [op r a1 a2]) (f))
        :when (not= return :Number)
        :let [an1 (gensym 'a)
              an2 (gensym 'b)
              [x] (run 1 [q]
                    (g op arg1 arg2 return an1 an2 () q))
              x (reverse x)]]
    (if (seq x)
      (str "public static " (name return) " " (name op) "(" (name arg1) " " an1 ", " (name arg2) " " an2 ") {\n"
           (with-out-str
             (doseq [i (butlast x)]
               (println " " i)))
           "  return " (last x)
           "\n}\n")
      (format "public static %s %s(%s a1, %s a2) FOO\n"
              (name return)
              (name op)
              (name arg1)
              (name arg2)))))
