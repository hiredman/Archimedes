(ns Archimedes.compiler.jvm3
  (:refer-clojure :exclude [compile])
  (:use [Archimedes.compiler]
        [clojure.java.io :only [copy file]]
        [clojure.contrib.logging :only [info]]
        [clojure.contrib.monads :only [state-m fetch-state]])
  (:import [clojure.asm ClassWriter Type MethodWriter Label]
           [clojure.asm.commons LocalVariablesSorter]
           [java.io ByteArrayOutputStream]
           [java.util.jar JarOutputStream JarInputStream]
           [java.util.zip ZipEntry]))

(defmacro op [name]
  `(. clojure.asm.Opcodes ~name))

(def primitive? #{:int :long})

(defn namespace-classwriter [namespace-symbol]
  (let [class-name (format "%s__init"
                           (.replaceAll (name namespace-symbol)
                                        "\\." "/"))]
    (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
      (.visit (op V1_5)
              (+ (op ACC_PUBLIC)
                 (op ACC_FINAL))
              class-name
              nil
              "java/lang/Object"
              nil))))

(defn function-classwriter [class-name]
  (let [class-name (.replaceAll class-name
                                "\\." "/")]
    (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
      (.visit (op V1_5)
              (+ (op ACC_PUBLIC)
                 (op ACC_FINAL))
              class-name
              nil
              "clojure/lang/AFn"
              nil))))

(defn function-ctor [class-writer class-name]
  (let [class-desc (.replaceAll class-name "\\." "/")
        method-writer (.visitMethod class-writer
                                    (op ACC_PUBLIC)
                                    "<init>"
                                    "()V"
                                    nil
                                    nil)]
    (doto method-writer
      (.visitIntInsn (op ALOAD) 0)
      (.visitMethodInsn (op INVOKESPECIAL)
                        "clojure/lang/AFn"
                        "<init>"
                        "()V")
      (.visitInsn (op RETURN))
      (.visitMaxs 0 0)
      (.visitEnd))))


(defn static-init [class-writer]
  (let [mw  (.visitMethod class-writer
                          (op ACC_STATIC)
                          "<clinit>"
                          "()V"
                          nil
                          nil)]
    (LocalVariablesSorter. (op ACC_STATIC) "()V" mw)))


(defmulti C (comp first first list))

(defn namespace-init-class [ns]
  (let [class-name (format "%s__init"
                           (name ns))
        file-name (.replaceAll class-name "\\." "/")
        class-writer (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                       (.visit (op V1_5)
                               (+ (op ACC_PUBLIC)
                                  (op ACC_FINAL))
                               file-name
                               nil
                               "java/lang/Object"
                               nil))]
    [class-name class-writer]))

(def ^{:dynamic true} *context* nil)

(defn f [fun]
  (let [c *context*]
    #(binding [*context* c]
       (fun))))

(defmulti safe-deref type)

(defmethod safe-deref clojure.lang.IDeref [x] @x)

(defmethod safe-deref :default [x] x)

(defrecord Namespace [class-writer method-writer class-name ns-name]
  clojure.lang.Named
  (getName [ns])
  clojure.lang.IDeref
  (deref [this]
    (doto method-writer
      (.visitInsn (op RETURN))
      (.visitMaxs 0 0)
      .visitEnd)
    (vector
     class-name
     (.toByteArray
      (doto class-writer
        .visitEnd)))))


(defn new-namespace [a-name]
  (let [[class-name class-writer] (namespace-init-class a-name)
        method-writer (static-init class-writer)]
    (Namespace.
     class-writer method-writer class-name a-name)))

(defmethod C :in-ns [[_ ns K] stack]
  (let [namespace (new-namespace ns)
        class-name (:class-name namespace)
        class-writer (:class-writer namespace)
        method-writer (:method-writer namespace)
        stack (conj stack namespace)]
    #(binding [*context* {:class-writer class-writer
                          :class-name class-name
                          :method-writer method-writer
                          :namespace ns
                          :vars #{}
                          :frame []}]
       (C (safe-deref K) stack))))

(defprotocol Ldc
  (ldc [value K stack context]))

(def signatures
  {'[String String Symbol]
   "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Symbol;"
   '[String String Var]
   "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"
   '[Object]
   "()Ljava/lang/Object;"
   '[Object Object]
   "(Ljava/lang/Object;)Ljava/lang/Object;"})

(extend-protocol Ldc
  nil
  (ldc [value K stack context]
    (.visitInsn (:method-writer context) (op ACONST_NULL))
    (f #(C (safe-deref K) stack)))
  Object
  (ldc [value K stack context]
    (.visitLdcInsn (:method-writer context) value)
    (f #(C (safe-deref K) stack)))
  clojure.lang.Symbol
  (ldc [value K stack context]
    (let [K [:constant (namespace value)
             [:constant (name value)
              [:invoke-static '[clojure.lang.Symbol/intern
                                [String String Symbol]]
               K]]]]
      (f #(C (safe-deref K) stack)))))

(defmethod C :constant [[_ v K] stack]
  (ldc v K stack *context*))

(defmulti box (comp first list))

(defmethod box Long [_ method-writer]
  (doto method-writer
    (.visitTypeInsn (op NEW) "java/lang/Long")
    (.visitInsn (op DUP))
    (.visitInsn (op DUP2_X2))
    (.visitInsn (op POP2))
    (.visitMethodInsn (op INVOKESPECIAL)
                      "java/lang/Long"
                      "<init>"
                      "(J)V")))

(defmethod C :box [[_ klass K] stack]
  (let [{:keys [method-writer]} *context*]
    (box Long method-writer)
    (f #(C (safe-deref K) stack))))

(defn make-var [method-writer]
  (.visitMethodInsn method-writer (op INVOKESTATIC)
                    "clojure/lang/RT"
                    "var"
                    (str "(Ljava/lang/String;"
                         "Ljava/lang/String;"
                         "Ljava/lang/Object;)"
                         "Lclojure/lang/Var;")))

(defn get-var [method-writer]
  (.visitMethodInsn method-writer (op INVOKESTATIC)
                    "clojure/lang/RT"
                    "var"
                    (str "(Ljava/lang/String;"
                         "Ljava/lang/String;)"
                         "Lclojure/lang/Var;")))

(defn deref-method [method-writer]
  (.visitMethodInsn method-writer (op INVOKEINTERFACE)
                    "clojure/lang/IDeref"
                    "deref"
                    "()Ljava/lang/Object;"))

(defmethod C :def [[_ a-name K] stack]
  (let [{:keys [method-writer]} *context*]
    (doto method-writer
      (.visitLdcInsn (namespace a-name))
      (.visitInsn (op SWAP))
      (.visitLdcInsn (name a-name))
      (.visitInsn (op SWAP))
      make-var)
    (f #(C (safe-deref K) stack))))

(defmethod C :halt [_ stack]
  (into {} (remove nil? (map deref stack))))

(defmethod C :deref [[_ var-name K] stack]
  ;;TODO: when-not vars contains? var-name, create static field
  (let [K [:constant (namespace var-name)
           [:constant (name var-name)
            [:invoke-static '[clojure.lang.RT/var
                              [String String Var]]
             [:invoke-interface '[clojure.lang.IDeref/deref
                                  [Object]]
              K]]]]]
    (f #(C (safe-deref K) stack))))

(defmethod C :cast [[_ klass K] stack]
  (let [{:keys [method-writer]} *context*]
    (doto method-writer
      (.visitTypeInsn (op CHECKCAST) (.replaceAll (name klass) "\\." "/")))
    (f #(C (safe-deref K) stack))))

(defmethod C :invoke [[_ arity K] stack]
  (let [{:keys [method-writer]} *context*
        method-desc (str "("
                         (apply str (repeat arity "Ljava/lang/Object;"))
                         ")Ljava/lang/Object;")]
    (doto method-writer
      (.visitMethodInsn (op INVOKEINTERFACE)
                        "clojure/lang/IFn"
                        "invoke"
                        method-desc))
    (f #(C (safe-deref K) stack))))

(defmethod C :frame [[_ locals K] stack]
  (let [{:keys [method-writer]} *context*
        locals (into {}
                     (for [l locals]
                       [l (.newLocal
                           method-writer (Type/getType "Ljava/lang/Object;"))]))]
    (binding [*context* (update-in *context* [:frame] conj locals)]
      (f #(C (safe-deref K) stack)))))

(defmethod C :pop-frame [[_ K] stack]
  (let [{:keys [method-writer frame]} *context*
        frame (peek frame)]
    (doseq [[v i] frame]
      (.visitInsn method-writer (op ACONST_NULL))
      (.visitVarInsn method-writer (op ALOAD) i))
    (binding [*context* (update-in *context* [:frame] pop)]
      (f #(C (safe-deref K) stack)))))

(defmethod C :bind [[_ a-name K] stack]
  (let [{:keys [method-writer frame]} *context*
        frame (peek frame)
        idx (get frame a-name)]
    (.visitVarInsn method-writer (op ASTORE) idx)
    (f #(C (safe-deref K) stack))))

(defmethod C :local [[_ a-name K] stack]
  (let [{:keys [method-writer frame]} *context*
        frame (peek frame)
        idx (get frame a-name)]
    (.visitVarInsn method-writer (op ALOAD) idx)
    (f #(C (safe-deref K) stack))))

(defmethod C :invoke-static [[_ [method-name method-sig] K] stack]
  (let [{:keys [method-writer frame]} *context*]
    (.visitMethodInsn method-writer (op INVOKESTATIC)
                      (.replaceAll (namespace method-name)
                                   "\\." "/")
                      (name method-name)
                      (signatures method-sig))
    (f #(C (safe-deref K) stack))))

(defmethod C :invoke-interface [[_ [method-name method-sig] K] stack]
  (let [{:keys [method-writer frame]} *context*]
    (.visitMethodInsn method-writer (op INVOKEINTERFACE)
                      (.replaceAll (namespace method-name)
                                   "\\." "/")
                      (name method-name)
                      (signatures method-sig))
    (f #(C (safe-deref K) stack))))

(defmethod C :fn-call [[_ K] stack]
  (let [K [:cast 'clojure.lang.IFn K]]
    (f #(C (safe-deref K) stack))))

(defmethod C :do [[_ K] stack]
  (doto (:method-writer *context*)
    (.visitInsn (op POP)))
  (f #(C (safe-deref K) stack)))

(defn function-context [context stack]
  (let [class-name (format "%s$%s"
                           (:class-name context)
                           (gensym 'fn))
        class-writer (function-classwriter class-name)]
    (merge context
           {:class-name class-name
            :class-writer class-writer
            :method-writer nil
            :return context})))

(defmethod C :fn [[_ K] stack]
  (let [old-context *context*
        context (function-context old-context stack)]
    (binding [*context* context]
      (f #(C (safe-deref K) stack)))))

(defmethod C :args [[_ args K] stack]
  (let [context (update-in *context* [:frame] conj
                           (zipmap args (map inc (range))))
        method-writer (.visitMethod (:class-writer context)
                                    (op ACC_PUBLIC)
                                    "invoke"
                                    (signatures '[Object Object])
                                    nil
                                    nil)]
    (binding [*context* (assoc context
                          :method-writer method-writer
                          :in-body true)]
      (f #(C (safe-deref K) stack)))))

(defmethod C :return [[_ K] stack]
  (if (:in-body *context*)
    (let [context (dissoc *context* :in-body)
          method-writer (:method-writer context)]
      (doto method-writer
        (.visitInsn (op ARETURN))
        (.visitMaxs 0 0)
        (.visitEnd))
      (binding [*context* (dissoc context :method-writer)]
        (f #(C (safe-deref K) stack))))
    (let [context *context*
          class-writer (:class-writer context)
          class-name (symbol (:class-name context))
          stack (conj stack (delay
                             [(name class-name) (.toByteArray
                                                 (doto class-writer
                                                   (.visitEnd)))]))
          K [:new [class-name []] K]]
      (function-ctor class-writer (:class-name context))
      (binding [*context* (:return context)]
        (f #(C (safe-deref K) stack))))))

(defmethod C :new [[_ [class-name sigs] K] stack]
  (let [method-writer (:method-writer *context*)]
    (doto method-writer
      (.visitTypeInsn (op NEW) (.replaceAll (name class-name) "\\." "/"))
      (.visitInsn (op DUP))
      (.visitMethodInsn (op INVOKESPECIAL)
                        (.replaceAll (name class-name) "\\." "/")
                        "<init>"
                        "()V"))
    (f #(C (safe-deref K) stack))))

(defn load-map [m]
  (doseq [[class-name class-bytes] m]
    (.defineClass
     (clojure.lang.DynamicClassLoader.
      (clojure.lang.RT/baseLoader))
     class-name class-bytes nil)))

(defmacro x []
  (pr-str (into {} (map (fn [[k v]] [k (.idx v)]) &env))))

(comment

  (do

    (in-ns 'foo.nar)

    ((fn* ([x] (println x))) 1)

    )
  (doto
      ((partial trampoline C)
       [:in-ns 'foo.baz
        [:deref 'clojure.core/in-ns
         [:constant 'foo.baz
          [:invoke 1
           [:fn '#{}
            [:args '[x]
             [:deref 'clojure.core/println
              [:fn-call
               [:local 'x
                [:invoke 1
                 [:return
                  [:close
                   [:fn-call
                    [:constant 5
                     [:box 'Long
                      [:invoke 1
                       [:halt]]]]]]]]]]]]]]]]]
         ())
    load-map)

  (+
   (try
     (+ 1 2)
     (catch Exception e
       0))
   5)

  [:deref 'clojure.core/+
   [:cast 'clojure.lang.IFn
    [:rescue '[Exception :FOO]
     [:deref 'clojure.core/+
      [:cast 'clojure.lang.IFn
       [:constant 1
        [:box 'Long
         [:constant 2
          [:box 'Long
           [:invoke 2
            [:begin-handler :FOO
             [:frame '[e]
              [:bind 'e
               [:constant 0
                [:pop-frame nil
                 [:end-handler :FOO
                  [:constant 5
                   [:box 'Long
                    [:invoke 2
                     [:halt]]]]]]]]]]]]]]]]]]]]

  )

(defmulti op first)

(defmethod op :halt [& _])

(defmethod op :refer [& _])

(defmethod op :constant [& _])

(defmethod op :closure [& _])

(defmethod op :test [& _])

(defmethod op :assign [& _])

;;fail
(defmethod op :conti [& _])
;;fail
(defmethod op :nuate [& _])

(defmethod op :frame [& _])

(defmethod op :argument [& _])

(defmethod op :apply [& _])

(defmethod op :return [& _])

[:frame
 [:refer "print"
 [:constant "hello world"
 [:argument
 [:apply 1]]]]
[:halt]]
