(ns Archimedes.asm
  (:refer-clojure :exclude [type])
  (:import [clojure.asm Opcodes Type]))

(defmacro op [name]
  `(. Opcodes ~name))

(defmacro type [nam]
  (let [name (symbol
              (format "%s_TYPE"
                      (.toUpperCase (name nam))))]
    `(. Type ~name)))

(defn method-desciptor [types]
  (let [args (when (> (count types) 1)
               (butlast types))
        args (into-array Type args)]
    (Type/getMethodDescriptor (last types) args)))

(defn header [cv name]
  (.visit cv
          (op V1_5)
          (+ (op ACC_PUBLIC)
             (op ACC_FINAL))
          name
          nil
          "java/lang/Object"
          nil))

(defn method [cw nam types]
  (.visitMethod cw
                (+ (op ACC_PUBLIC)
                   (op ACC_STATIC)
                   (op ACC_FINAL))
                (name nam)
                (method-desciptor types)
                nil
                nil))

(defn goto [method-visitor label]
  (.visitJumpInsn method-visitor (op GOTO) label))

(defn ldc [method-visitor c]
  (.visitLdcInsn method-visitor c))

(defn label [method-visitor label]
  (.visitLabel method-visitor label))

(comment
  (defrecord Instruction [n args]
    clojure.lang.IFn
    (invoke
     [i method-writer]
     (case (count args)
           0
           (.visitInsn
            method-writer
            (clojure.lang.Reflector/getStaticField
             "clojure.asm.Type"
             (name n)))
           1
           (.visitIntInsn ))))

  (defrecord Method [access mname signature instructions]
    clojure.lang.IFn
    (invoke
     [m cw]
     (let [signature (map #(.getDescriptor %) signature)
           args (butlast signature)
           return (last signature)
           args (reduce str "" args)
           method (.visitMethod
                   cw (reduce + access) (name mname) (format "(%s)%s" args return)
                   nil nil)]
       (.visitCode method)
       (.visitMaxs method 0 0)
       (.visitEnd method)))))
