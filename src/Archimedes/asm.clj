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
