(ns Archimedes.compiler
  (:use [clojure.java.io :only [copy file]])
  (:import [clojure.asm ClassWriter Type]))

(defmacro op [name]
  `(. clojure.asm.Opcodes ~name))

(defprotocol Context
  (init [this values])
  (start-procedure [this name attrs] "returns an next context")
  (end-procedure [this] "pops context ")
  (burn [this])
  (bytes [this])
  (file-name [this])
  (function-call [this args])
  (resolve-var [this var])
  (loadc [this value])
  (return [this]))

(defprotocol Types
  (Obj [this])
  (Str [this])
  (Var [this])
  (VVoid [this]))

(defrecord JVMTypes []
  Types
  (Obj [this] (Type/getType "Ljava/lang/Object;"))
  (Str [this] (Type/getType "Ljava/lang/String;"))
  (Var [this] (Type/getType "Lclojure/lang/Var;"))
  (VVoid [this] Type/VOID_TYPE))

(defn method-desciptor [types]
  (let [args (when (> (count types) 1)
               (butlast types))
        args (into-array Type args)]
    (Type/getMethodDescriptor (last types) args)))

(defrecord JVMCntxt [class-writer method-writer variable-stack
                     context-stack]
  Context
  (init
   [this values]
   (.visit class-writer
           (op V1_5)
           (+ (op ACC_PUBLIC)
              (op ACC_FINAL))
           (:name values)
           nil
           (:super values)
           nil)
   (into this values))
  (start-procedure
   [this name attrs]
   (let [method-writer (.visitMethod class-writer
                                     (:access attrs)
                                     name
                                     (method-desciptor
                                      (:method-descriptor attrs))
                                     nil
                                     nil)]
     (.visitCode method-writer)
     (reduce into
             (assoc (update-in this [:context-stack] conj this)
               :method-writer method-writer)
             [{:method-name name}
              attrs])))
  (end-procedure
   [this]
   (.visitMaxs method-writer 0 0)
   (.visitEnd method-writer)
   (peek (:context-stack this)))
  (burn
   [this]
   (when-not (:ctor this)
     (let [method-writer (.visitMethod class-writer
                                       (op ACC_PUBLIC)
                                       "<init>"
                                       (method-desciptor [(VVoid (JVMTypes.))])
                                       nil
                                       nil)]
       (.visitCode method-writer)
       (.visitVarInsn method-writer (op ALOAD) 0)
       (.visitMethodInsn method-writer
                         (op INVOKESPECIAL)
                         (:super this)
                         "<init>"
                         (method-desciptor [(VVoid (JVMTypes.))]))
       (.visitInsn method-writer (op RETURN))
       (.visitMaxs method-writer 0 0)
       (.visitEnd method-writer)))
   (.visitEnd class-writer))
  (bytes
   [this]
   (.toByteArray class-writer))
  (file-name
   [this]
   (:name this))
  (function-call
   [this argc]
   (.visitMethodInsn method-writer
                     (op INVOKEINTERFACE)
                     "clojure/lang/IFn"
                     "invoke"
                     (method-desciptor (repeat (inc argc) (Obj (JVMTypes.)))))
   this)
  (resolve-var
   [this var]
   (.visitLdcInsn method-writer (name (.getName (.ns var))))
   (.visitLdcInsn method-writer (name (.sym var)))
   (.visitMethodInsn method-writer
                     (op INVOKESTATIC)
                     "clojure/lang/RT"
                     "var"
                     (method-desciptor  [(Str (JVMTypes.))
                                         (Str (JVMTypes.))
                                         (Var (JVMTypes.))]))
   (.visitMethodInsn method-writer
                     (op INVOKEINTERFACE)
                     "clojure/lang/IDeref"
                     "deref"
                     (method-desciptor  [(Obj (JVMTypes.))]))
   (when (:fn-call this)
     (.visitTypeInsn method-writer (op CHECKCAST) "clojure/lang/IFn"))
   this)
  (loadc
   [this value]
   (.visitLdcInsn method-writer value)
   (cond
    (instance? Integer value)
    (doto method-writer
      (.visitTypeInsn (op NEW) "java/lang/Integer")
      (.visitInsn (op DUP_X1))
      (.visitInsn (op SWAP))
      (.visitMethodInsn
       (op INVOKESPECIAL)
       "java/lang/Integer"
       "<init>"
       (method-desciptor [Type/INT_TYPE (VVoid (JVMTypes.))]))))
   this)
  (return
   [this]
   (.visitInsn method-writer (op ARETURN))
   this))

(def compile* (fn [x c] c))

(defprotocol Compilable
  (compile* [expr cxt]))

(extend-protocol Compilable
  clojure.lang.ISeq
  (compile*
   [expr cxt]
   (let [context (dissoc (compile* (first expr) (assoc cxt :fn-call true))
                           :fn-call)
           context (reduce #(compile* %2 %) context (rest expr))]
       (function-call context (dec (count expr)))))
  clojure.lang.Symbol
  (compile*
   [exp cxt]
   (resolve-var cxt (resolve exp)))
  java.lang.Number
  (compile*
   [exp cxt]
   (loadc cxt exp)))

(defn compile** [exp cxt]
  (return (compile* exp cxt)))

(defn f []
  (let [jvm (JVMTypes.)
        class-name "Archimedes/_15"
        context (JVMCntxt. (ClassWriter. ClassWriter/COMPUTE_FRAMES) nil [] [])
        context (init context {:name class-name :super "clojure/lang/AFn"})
        context (start-procedure context "invoke"
                                 {:method-descriptor [(Obj jvm)]
                                  :access (op ACC_PUBLIC)})
        context (compile** '(+ 3 (+ 1 2)) context)
        context (end-procedure context)]
    (burn context)
    (copy (bytes context) (file "/tmp/test.class"))
    (bytes context)))
