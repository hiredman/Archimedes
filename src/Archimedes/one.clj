(ns Archimedes.one
  (:refer-clojure :exclude [type])
  (:use [Archimedes.asm])
  (:import [clojure.asm ClassWriter]
           [clojure.asm.commons Method GeneratorAdapter])
  (:require [clojure.zip :as zip]))

(def c (atom 0))

(defn classname []
  (swap! c inc)
  (str "foo" @c))

(def code "(fn [^int x] ^int (+ x 1))")

(def ops {['+ 'int 'int] [:iadd 'int]
          [:load 'int] []})

(def type-map {'int (type int)})

(defmulti compile (comp first last list))

(defmethod compile 'fn [types locations [_ args body]]
  (let [class-name (classname)
        class-writer (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                       (.visit (op V1_5)
                               (+ (op ACC_PUBLIC)
                                  (op ACC_FINAL))
                               class-name
                               nil
                               "clojure/lang/AFn"
                               nil))
        return (:tag (meta body))
        types (into types (for [sym args] [sym (:tag (meta sym))]))
        locations (into locations (zipmap args (range)))
        arg-types (map types args)
        method-signature (format "%s invoke (%s)"
                                 (reduce
                                  str
                                  arg-types)
                                 return)
        method (Method/getMethod method-signature)
        generator (GeneratorAdapter.
                   (op ACC_PUBLIC) method nil nil class-writer)]
    (doto generator
      (.loadArg 0)
      (.visitLdcInsn 1)
      (.math GeneratorAdapter/ADD (return type-map))
      (.returnValue)
      (.endMethod))
    (.visitEnd class-writer)
    (.toByteArray class-writer)))

(defn f []
  (let [code (read-string code)]
    (compile {} {} code)))

(defn h [known code]
  (if (seq? code)
    (condp = (first code)
          '+
          (assoc (into known (for [x (rest code)] [x :integer]))
            code :integer)
          'fn
          (let [[_ args body] code
                known1 (h (apply dissoc known args) body)
                args (vec (map known1 args))]
            (assoc known code (conj args (known1 body)))))
    known))

(defn g []
  (let [code (zip/next (zip/seq-zip '(fn [x y] (+ x y))))]
    (loop [c code types {}]
      (cond
       (zip/end? c)
       (zip/root c)
       (and (seq? (zip/node c))
            (= '+ (first (zip/node c)))
            (:tag (meta (zip/node c))))
       (let [n (zip/node c)
             types (assoc types (first n) 'int (second n) 'int)]
         (recur (-> c (zip/replace (vary-meta n assoc :tag 'int)) zip/root
                    zip/seq-zip) types))
       (and (symbol? (zip/node c))
            (:tag (meta (zip/node c))))
       (let [sym (zip/node c)]
         (println sym)
         (if (contains? types sym)
           (recur (-> c (zip/replace (vary-meta sym assoc :tag (types sym))
                                     zip/root zip/seq-zip))
                  types)
           (recur (zip/next c) types)))
       :else
        (do
          (println (zip/node c))
          (recur (zip/next c) types))))))


(defmulti type-of (fn [exp tenv] (type exp)))

(defmethod type-of Number [exp tenv] :integer)

(defmethod type-of )
