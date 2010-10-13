(ns Archimedes.compiler.jvm2
  (:refer-clojure :exclude [compile])
  (:use [Archimedes.compiler]
        [clojure.java.io :only [copy file]]
        [clojure.contrib.logging :only [info]]
        [clojure.contrib.monads :only [state-m fetch-state]])
  (:import [clojure.asm ClassWriter Type MethodWriter]
           [java.io ByteArrayOutputStream]
           [java.util.jar JarOutputStream JarInputStream]
           [java.util.zip ZipEntry]))

(defmacro op [name]
  `(. clojure.asm.Opcodes ~name))

(def primitive? #{:int :long})

(defn current-methodwriter [stack]
  (first (filter #(= MethodWriter (type %)) (reverse stack))))

(defn box [type]
  (condp = type
      :int
    (in state-m
      (fetch-state) :as stack
      (return (current-methodwriter stack)) :as method-writer
      (return (doto method-writer
                (.visitTypeInsn (op NEW) "java/lang/Integer")
                (.visitInsn (op DUP_X1))
                (.visitInsn (op SWAP))
                (.visitMethodInsn (op INVOKESPECIAL)
                                  "java/lang/Integer"
                                  "<init>"
                                  "(I)V")))
      (return :Integer))

    :long
    (in state-m
      (fetch-state) :as stack
      (return (current-methodwriter stack)) :as method-writer
      (return (doto method-writer
                (.visitTypeInsn (op NEW) "java/lang/Long")
                (.visitInsn (op DUP))
                (.visitInsn (op DUP2_X2))
                (.visitInsn (op POP2))
                (.visitMethodInsn (op INVOKESPECIAL)
                                  "java/lang/Long"
                                  "<init>"
                                  "(J)V")))
      (return :Long))))



(defrecord Init [machine values])
(defrecord Fin [machine values])
(defrecord Produce [machine args])
(defrecord StartProcedure [machine name attrs])
(defrecord EndProcedure [machine])
(defrecord ProcedureCall [machine name args])
(defrecord FunctionCall [machine args])
(defrecord ResolveVar [machine var])
(defrecord AccessLocal [machine local])
(defrecord Immediate [machine value attrs])
(defrecord DefineFunction [machine attrs])
(defrecord StartNamespace [machine namespace])
(defrecord Define [machine name value])

(defrecord Namespace [name mappings aliases])

(defrecord CWriter [cw class-name])

(defn current-namespace [stack]
  (first (filter #(= Namespace (type %)) (reverse stack))))

(defn current-classwriter [stack]
  (:cw (first (filter #(= CWriter (type %)) (reverse stack)))))



(defn namespace-classwriter [namespace-symbol]
  (let [class-name (format "%s__init"
                           (.replaceAll (name namespace-symbol)
                                        "\\." "/"))]
    (CWriter. (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                (.visit (op V1_5)
                        (+ (op ACC_PUBLIC)
                           (op ACC_FINAL))
                        class-name
                        nil
                        "java/lang/Object"
                        nil))
              (.replaceAll class-name "/" "."))))

(defn static-init []
  (in state-m
    (fetch-state) :as stack
    (return (current-classwriter stack)) :as class-writer
    (let [method-writer (.visitMethod class-writer
                                      (op ACC_STATIC)
                                      "<clinit>"
                                      "()V"
                                      nil
                                      nil)]
      (return method-writer)) :as method-writer
      (update-state conj method-writer)))

(defrecord JVM []
  Machine
  (init [machine values]
    (in state-m
      (update-state conj (Init. machine values))))
  (fin [machine values]
    (in state-m
      (fetch-state) :as state
      (return
       (reduce
        (fn [acc thing]
          (if (= MethodWriter (type thing))
            (do
              (doto thing
                (.visitInsn (op RETURN))
                (.visitMaxs 0 0)
                (.visitEnd))
              acc)
            (conj acc thing)))
        []
        state)) :as new-state
        (return
         (reduce
          (fn [acc thing]
            (if (= CWriter (type thing))
              (do
                (doto (:cw thing)
                  (.visitEnd))
                (conj acc (update-in thing [:cw] #(.toByteArray %))))
              (conj acc thing)))
          []
          new-state)) :as new-state
          (update-state (constantly new-state))))
  (produce [machine args]
    (in state-m
      (fetch-state) :as state
      (return
       (with-open [baos (ByteArrayOutputStream.)
                   jaros (JarOutputStream. baos)]
         (reduce
          (fn [jaros thing]
            (when (= CWriter (type thing))
              (.putNextEntry jaros (ZipEntry. (format "%s.class" (.replaceAll (:class-name thing) "\\." "/"))))
              (copy (:cw thing) jaros)
              (.closeEntry jaros))
            jaros)
          jaros
          state)
         (.close jaros)
         (.toByteArray baos)))))

  (type-of [machine name] :x)
  (start-procedure [machine name attrs]
    (in state-m
      (update-state conj (StartProcedure. machine name attrs))))

  (end-procedure [machine]
    (in state-m
      (update-state conj (EndProcedure. machine))))

  (procedure-call [machine name args]
    (in state-m
      (update-state conj (ProcedureCall. machine name args))))

  (function-call [machine args]
    (in state-m
      (update-state conj (FunctionCall. machine args))))

  (resolve-var [machine var]
    (in state-m
      (update-state conj (ResolveVar. machine var))))

  (access-local [machine local]
    (in state-m
      (update-state conj (AccessLocal. machine local))))

  (immediate [machine value attrs]
    (condp #(isa? %2 %) (type value)
      Number
      (in state-m
        (fetch-state) :as stack
        (return (current-methodwriter stack)) :as method-writer
        (return nil)
        (return
         (doto method-writer
           (.visitLdcInsn value)))
        (return :long)
        (return :long))
      String
      (in state-m
        (fetch-state) :as stack
        (return (current-methodwriter stack)) :as method-writer
        (return nil)
        (return
         (doto method-writer
           (.visitLdcInsn value)))
        (return :String)
        (return :String))))

  (define-function [machine attrs]
    (in state-m
      (update-state conj (DefineFunction. machine attrs))))
  
  (start-namespace [machine namespace]
    (let [ns (Namespace. namespace (atom #{}) (atom {}))]
      (in state-m
        (update-state conj (namespace-classwriter namespace))
        (update-state conj ns)
        (static-init)
        (return ns))))
  
  (define [machine name value]
    (in state-m
      (fetch-state) :as stack
      (return (current-namespace stack)) :as ns
      (compile (str (:name ns)) machine)
      (compile (clojure.core/name name) machine)
      (compile value machine) :as type
      (if (primitive? type)
        (box type)
        (return type))
      (return
       (update-in ns [:mappings] swap! conj (vary-meta name update-in [:tag] (fn [tag] (if tag tag type)))))
      (fetch-state) :as state
      (return (current-methodwriter stack)) :as method-writer
      (return (doto method-writer
                (.visitMethodInsn (op INVOKESTATIC)
                                  "clojure/lang/RT"
                                  "var"
                                  (str "(Ljava/lang/String;"
                                       "Ljava/lang/String;"
                                       "Ljava/lang/Object;)"
                                       "Lclojure/lang/Var;"))))
      (return :Var))))


(defn jvm []
  (JVM.))

(comment

  ((in state-m
     (reduce
      (fn [m form]
        (bind m (fn [_] (compile form (jvm)))))
      (return nil)
      '[(in-ns 'foo.bar)
        (def x 1)])
     (fin (jvm) nil)
     (produce (jvm) nil) :as x
     (return x))
   [])

  )
