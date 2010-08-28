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
                      (let [m (method cv name (repeat (inc argc) (asmType t)))]
                        (dotimes [i argc]
                          (Tload t (* i (Tsize t)) m))
                        (let [rt (gen t m)]
                          (return rt m))
                        (.visitMaxs m 0 0)
                        (.visitEnd m)))))
                 .visitEnd))]
    (.mkdirs (file *compile-path* "Archimedes"))
    (copy bytes (file *compile-path* "Archimedes/Ops.class"))))
