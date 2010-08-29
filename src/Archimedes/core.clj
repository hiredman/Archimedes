(ns Archimedes.core
  (:use [clojure.java.io :only [file copy]]
        [Archimedes.types]
        [Archimedes.asm])
  (:refer-clojure :exclude [type])
  (:import [clojure.asm ClassWriter Opcodes Type]))


(when *compile-files*
  (let [name "Archimedes/Ops"
        bytes (.toByteArray
               (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                 (header name)
                 ((fn [cv]
                    (doseq [t (types)
                            [name gen argc] (method-list)]
                      (gen t nil2 cv))))
                 .visitEnd))]
    (.mkdirs (file *compile-path* "Archimedes"))
    (copy bytes (file *compile-path* "Archimedes/Ops.class"))))
