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



(defrecord EndProcedure [machine])
(defrecord ProcedureCall [machine name args])
(defrecord AccessLocal [machine local])
(defrecord DefineFunction [machine attrs])
(defrecord Locals [arglist])

(defrecord Scope [name])

(defrecord Namespace [name mappings aliases])

(defrecord Var [namespace name])

(defrecord CWriter [cw class-name])

(defn current-namespace [stack]
  (first (filter #(= Namespace (type %)) (reverse stack))))

(defn current-classwriter [stack]
  (:cw (first (filter #(= CWriter (type %)) (reverse stack)))))

(defn current-class-name [stack]
  (:class-name (first (filter #(= CWriter (type %)) (reverse stack)))))

(defn current-scope [stack]
  (first (filter #(= Scope (type %)) (reverse stack))))

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

(defn function-classwriter [class-name]
  (let [class-name (.replaceAll class-name
                                "\\." "/")]
    (CWriter. (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                (.visit (op V1_5)
                        (+ (op ACC_PUBLIC)
                           (op ACC_FINAL))
                        class-name
                        nil
                        "clojure/lang/AFn"
                        nil))
              (.replaceAll class-name "/" "."))))

(defn method-call [op owner name desc]
  (in state-m
    (fetch-state) :as stack
    (return (current-methodwriter stack)) :as method-writer
    (return (doto method-writer
              (.visitMethodInsn op owner name desc)))))

(defn default-ctor [class-desc]
  (in state-m
    (fetch-state) :as stack
    (return (current-classwriter stack)) :as class-writer
    (let [method-writer (.visitMethod class-writer
                                      (op ACC_PUBLIC)
                                      "<init>"
                                      "()V"
                                      nil
                                      nil)]
      (return method-writer)) :as method-writer
      (update-state conj method-writer)
      (return (doto method-writer
                (.visitIntInsn (op ALOAD) 0)))
      (method-call (op INVOKESPECIAL)
                   class-desc
                   "<init>"
                   "()V")
      (update-state pop)
      (return (doto method-writer
                (.visitInsn (op RETURN))
                (.visitMaxs 0 0)
                (.visitEnd)))
      (return nil)))

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

(defn function-methodwriter [argc]
  (in state-m
    (fetch-state) :as stack
    (return (current-classwriter stack)) :as class-writer
    (return (case argc
                  1
                  (.visitMethod class-writer
                                (op ACC_PUBLIC)
                                "invoke"
                                "(Ljava/lang/Object;)Ljava/lang/Object;"
                                nil
                                nil))) :as method-writer
                                (update-state conj method-writer)))

(defrecord JVM [jaros baos]
  Machine

  (init [machine values]
    (in state-m
      (update-state conj :init)))

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
       (do
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
         (.close baos)
         (.toByteArray baos)))))

  (type-of [machine name] :x)

  (start-procedure [machine name attrs]
    (in state-m
      (update-state conj :start-proc)))

  (end-procedure [machine]
    (in state-m
      (update-state conj :end-proc)))

  (procedure-call [machine name args]
    (in state-m
      (update-state conj (ProcedureCall. machine name args))))

  (function-call [machine args]
    (in state-m
      (fetch-state) :as stack
      (return (current-methodwriter stack)) :as method-writer
      (case args
            1
            (method-call (op INVOKEINTERFACE)
                         "clojure/lang/IFn"
                         "invoke"
                         "(Ljava/lang/Object;)Ljava/lang/Object;"))
      (return :Object)))

  (resolve-var [machine var]
    (when-not (symbol? var)
      (throw (Exception. "not a symbol")))
    (in state-m
      (fetch-state) :as stack
      (return (current-namespace stack)) :as ns
      (return (@(:mappings ns) var)) :as var
      (immediate machine (:namespace var) nil)
      (immediate machine (:name var) nil)
      (method-call (op INVOKESTATIC)
                   "clojure/lang/RT"
                   "var"
                   (str "(Ljava/lang/String;"
                        "Ljava/lang/String;)"
                        "Lclojure/lang/Var;"))
      (method-call (op INVOKEINTERFACE)
                   "clojure/lang/IDeref"
                   "deref"
                   "()Ljava/lang/Object;")
      (if (= :fn-call (peek stack))
        (in state-m
          (return (doto (current-methodwriter stack)
                    (.visitTypeInsn (op CHECKCAST) "clojure/lang/IFn")))
          (update-state pop)
          (return :Fn))
        (return :Object))))

  (access-local [machine local]
    (info "@access-local" local)
    (in state-m
      (fetch-state) :as stack
      (return (.indexOf (:arglist (first (filter #(= Locals (type %)) (reverse stack)))) local)) :as local-index
      (return (info (str "local-index: " local-index)))
      (return (doto (current-methodwriter stack)
                (.visitIntInsn (op ALOAD) (inc local-index))))))

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

  (start-function [machine args]
    (info "@start-function")
    (in state-m
      (fetch-state) :as stack
      (return (current-scope stack)) :as scope
      (return (format "%s$%s" (:name scope) (gensym 'fn))) :as fn-name
      (update-state conj
                    (function-classwriter fn-name)
                    (Scope. fn-name)
                    (Locals. args))
      (function-methodwriter (count args))))

  (end-function [machine args]
    (info "@end-function")
    (in state-m
      (fetch-state) :as stack
      (return (current-methodwriter stack)) :as method-writer
      (return (doto method-writer
                (.visitInsn (op ARETURN))
                (.visitMaxs 0 0)
                (.visitEnd)))
      (default-ctor "clojure/lang/AFn")
      (return (current-classwriter stack)) :as class-writer
      (return (doto class-writer (.visitEnd)))
      (return (current-class-name stack)) :as fn-class-name
      (return (do
                (.putNextEntry
                 jaros
                 (ZipEntry.
                  (format
                   "%s.class"
                   (.replaceAll
                    fn-class-name "\\." "/"))))
                (copy (.toByteArray class-writer) jaros)
                (.closeEntry jaros)))
      (update-state (comp pop pop pop pop))          ;method writer
      (fetch-state) :as stack
      (return (current-methodwriter stack)) :as method-writer
      (return (doto method-writer
                (.visitTypeInsn (op NEW) (.replaceAll fn-class-name "\\." "/"))
                (.visitInsn (op DUP))))
      (method-call (op INVOKESPECIAL)
                   (.replaceAll fn-class-name "\\." "/")
                   "<init>"
                   "()V")
      (return nil)))

  (start-namespace [machine namespace]
    (let [ns (Namespace. namespace (atom {}) (atom {}))]
      (in state-m
        (update-state conj (namespace-classwriter namespace) ns (Scope. (name namespace)))
        (static-init)
        (return ns))))

  (refer-to [machine namespace]
    (in state-m
      (fetch-state) :as stack
      (return (current-namespace stack)) :as ns
      (return
       (update-in ns [:mappings] swap! into (map (fn [[n v]] [n (Var. (name (.getName (.ns v))) (name (.sym v)))]) (ns-publics (create-ns namespace)))))
      (return nil)))

  (local? [machine name]
    (info "@local?" name)
    (in state-m
      (fetch-state) :as stack
      (return (first (filter #(and (= Locals (type %)) (some #{name} (:arglist %))) stack))) :as locals
      (return (boolean locals))))

  (define [machine name value]
    (info (str "Define " name " " value))
    (in state-m
      (fetch-state) :as stack
      (return (current-namespace stack)) :as ns
      ;;need the namespace and symbol
      (compile (str (:name ns)) machine)
      (compile (clojure.core/name name) machine)
      (compile value machine) :as type
      ;;box it if it's primitive, vars hold objects
      (if (primitive? type)
        (box type)
        (return type))
      (return
       (update-in ns [:mappings] swap! assoc (vary-meta name update-in [:tag] (fn [tag] (if tag tag type))) (Var. (str (:name ns)) (clojure.core/name name))))
      (fetch-state) :as state
      (return (current-methodwriter stack)) :as method-writer
      (method-call (op INVOKESTATIC)
                   "clojure/lang/RT"
                   "var"
                   (str "(Ljava/lang/String;"
                        "Ljava/lang/String;"
                        "Ljava/lang/Object;)"
                        "Lclojure/lang/Var;"))
      (return (info "Defined"))
      (return :Var))))


(defn jvm []
  (let [baos (ByteArrayOutputStream.)
        jaros (JarOutputStream. baos)]
    (JVM. jaros baos)))

(comment

  (let [jvm (jvm)
        [bytes _] ((in state-m
                     (return (info "Starting Run"))
                     (reduce
                      (fn [m form]
                        (bind m (fn [_] (compile form jvm))))
                      (return nil)
                      '[(in-ns 'foo.bar)
                        (refer 'clojure.core)
                        (def x 1)
                        (def f (fn* ([x] x)))
                        (println (f x))])
                     (fin jvm nil)
                     (produce jvm nil) :as x
                     (return x))
                   [])]
    (info "End Run")
    (copy bytes (file "/tmp/foo.jar")))

  )
