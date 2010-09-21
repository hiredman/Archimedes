(ns Archimedes.core
  (:use [clojure.java.io :only [file copy]]
        [Archimedes.types]
        [Archimedes.asm]
        [clojure.set])
  (:refer-clojure :exclude [type])
  (:import [clojure.asm ClassWriter Opcodes Type]
           [Archimedes.asm Method]))

(def class-name "Archimedes.Ops")

(def m (Method. [(op ACC_PUBLIC) (op ACC_FINAL)]
                :add [Type/INT_TYPE Type/INT_TYPE Type/INT_TYPE] nil))

(when *compile-files*
    (let [name (.replaceAll class-name "\\." "/")
          class-writer (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                         (header name))]
      (visit m class-writer)
      (.visitEnd class-writer)
      (let [bytes (.toByteArray class-writer)]
        (.mkdirs (file *compile-path* (first (.split name "/"))))
        (copy bytes (file *compile-path* (str name ".class"))))))
