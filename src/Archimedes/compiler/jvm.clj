(ns Archimedes.compiler.jvm
  (:use [Archimedes.compiler]
        [clojure.java.io :only [copy file]]
        [clojure.contrib.logging :only [info]])
  (:import [clojure.asm ClassWriter Type]
           [java.io ByteArrayOutputStream]
           [java.util.jar JarOutputStream]
           [java.util.zip ZipEntry]))

(defmacro op [name]
  `(. clojure.asm.Opcodes ~name))

(def types
  {:obj (Type/getType "Ljava/lang/Object;")
   :str (Type/getType "Ljava/lang/String;")
   :var (Type/getType "Lclojure/lang/Var;")
   :int (Type/getType "Ljava/lang/Integer;")
   :number (Type/getType "Ljava/lang/Number;")
   :fn (Type/getType "Lclojure/lang/IFn;")
   :void Type/VOID_TYPE
   :symbol (Type/getType "Lclojure/lang/Symbol;")
   :namespace (Type/getType "Lclojure/lang/Namespace;")})

(defn method-desciptor [types]
  (let [args (when (> (count types) 1)
               (butlast types))
        args (into-array Type args)]
    (Type/getMethodDescriptor (last types) args)))

(defn method-call [method-writer op method arg-types]
  (let [owner (.replaceAll (namespace method) "\\." "/")
        method (name method)]
    (.visitMethodInsn method-writer
                      op
                      owner
                      method
                      (method-desciptor (map #(types % %) arg-types)))))

(defn load-var [method-writer]
  (method-call method-writer
               (op INVOKESTATIC)
               :clojure.lang.RT/var
               [:str :str :var]))

(defn object-array [method-writer size]
  (.visitLdcInsn method-writer size)
  (.visitTypeInsn method-writer (op ANEWARRAY) "java/lang/Object")
  (dotimes [i size]
    (doto method-writer
      (.visitInsn (op DUP_X1))
      (.visitInsn (op SWAP))
      (.visitLdcInsn (dec (- size i)))
      (.visitInsn (op SWAP))
      (.visitInsn (op AASTORE)))))

(defn default-ctor [machine]
  (let [method-writer (.visitMethod (:class-writer machine)
                                    (op ACC_PUBLIC)
                                    "<init>"
                                    (method-desciptor [(type-of machine :void)])
                                    nil
                                    nil)]
    (.visitCode method-writer)
    (.visitVarInsn method-writer (op ALOAD) 0)
    (.visitMethodInsn method-writer
                      (op INVOKESPECIAL)
                      (:super machine)
                      "<init>"
                      (method-desciptor [(type-of machine :void)]))
    (.visitInsn method-writer (op RETURN))
    (.visitMaxs method-writer 0 0)
    (.visitEnd method-writer)))

(defrecord PlaceHolder [dummy])

(defrecord JVM [types class-writer method-writer namespaces]
  Machine

  (init
   [machine values]
   (info "Init machine.")
   ;;initialize machine
   (.visit class-writer
           (op V1_5)
           (+ (op ACC_PUBLIC)
              (op ACC_FINAL))
           (:name values)
           nil
           (:super values)
           nil)
   (let [baos (ByteArrayOutputStream.)
         jaros (JarOutputStream. baos)]
     (assoc machine :baos baos :jaros jaros
            :name (:name values)
            :super (:super values))))

  (type-of
   [machine name]
   ;;map symbolic names to machine types
   (name types))

  (start-procedure
   [machine name attrs]
   (info (format "End proc: %s" name))
   ;;start a primitive (non-closure) procedure
   (let [method-writer (.visitMethod class-writer
                                     (:access attrs)
                                     name
                                     (method-desciptor
                                      (:method-descriptor attrs))
                                     nil
                                     nil)]
     (.visitCode method-writer)
     (reduce into
             (assoc machine
               :previous-machine machine
               :method-writer method-writer)
             [{:method-name name}
              attrs])))

  (end-procedure
   [machine]
   (info "End proc")
   ;;end the definition of a primitive procedure
   (.visitMaxs method-writer 0 0)
   (.visitEnd method-writer)
   (:previous-machine machine))

  (procedure-call
   [machine a-name args]
   (info (format "Proc call: %s" a-name))
   ;;call a primitive procedure
   ;;TODO: non-reflective calls
   ;;TODO: non-static calls
   ;;TODO: logic for static/instance methods should be inside here
   ;;TODO: don't box return values/deal with primitives properly
   (letfn [(box
            [method-writer return]
            (if (not= (.getSort return) Type/OBJECT)
              (do
                (case (.getSort return)
                      5;int
                      (do
                        (doto method-writer
                          (.visitTypeInsn (op NEW) "java/lang/Integer")
                          (.visitInsn (op DUP_X1))
                          (.visitInsn (op SWAP))
                          (.visitMethodInsn
                           (op INVOKESPECIAL)
                           "java/lang/Integer"
                           "<init>"
                           (method-desciptor [Type/INT_TYPE (:void types)])))
                        (Type/getType "Ljava/lang/Integer;"))))
              return))
           (prep-return
            [machine argc return]
            (let [return (box method-writer return)]
              (update-in machine [:variable-stack]
                         (fn [stack]
                           (conj (nth (iterate pop stack) argc)
                                 return)))))
           (find-method
            [klass name args]
            (->> (.getMethods klass)
                 (filter #(= name (.getName %)))
                 (filter #(or (= (vec args) (vec (Type/getArgumentTypes %)))
                              (= (vec (repeat (count args)
                                              (type-of machine :obj)))
                                 (vec (Type/getArgumentTypes %)))))
                 first))]
     (let [klass (Class/forName (namespace a-name))
           method (name a-name)
           argc (:argc args)
           arg-types (loop [i 0 s (:variable-stack machine) r ()]
                       (if (> argc i)
                         (recur (inc i) (pop s) (conj r (peek s)))
                         r))
           M (find-method klass method arg-types)
           machine (if (:static args)
                     (if M
                       (do
                         (doto method-writer
                           (method-call (op INVOKESTATIC)
                                        a-name
                                        (concat (Type/getArgumentTypes M)
                                                [(Type/getReturnType M)])))
                         (prep-return machine argc (Type/getReturnType M)))
                       (do
                         (object-array method-writer argc)
                         (doto method-writer
                           (.visitLdcInsn (namespace a-name))
                           (method-call
                            (op INVOKESTATIC)
                            :java.lang.Class/forName
                            [(Type/getType "Ljava/lang/String;")
                             (Type/getType "Ljava/lang/Class;")])
                           (.visitInsn (op SWAP))
                           (.visitLdcInsn (name method))
                           (.visitInsn (op SWAP))
                           (method-call
                            (op INVOKESTATIC)
                            :clojure.lang.Reflector/invokeStaticMethod
                            [(Type/getType "Ljava/lang/Class;")
                             (Type/getType "Ljava/lang/String;")
                             (Type/getType "[Ljava/lang/Object;")
                             (:obj types)]))
                         (prep-return machine argc (type-of machine :obj)))))]
       (if (:do machine)
         (do
           (.visitInsn method-writer (op POP))
           (update-in machine [:variable-stack] pop))
         machine))))

  (function-call
   [machine args]
   (info "Call function")
   ;;invoke a clojure function object
   (method-call method-writer
                (op INVOKEINTERFACE)
                :clojure.lang.IFn/invoke
                (repeat (inc args) :obj))
   (if (:do machine)
     (do
       (.visitInsn method-writer (op POP))
       machine)
     (update-in machine [:variable-stack] conj (type-of machine :obj))))

  (resolve-var
   [machine var]
   (info (format "Resolve var: %s" var))
   ;;TODO: how to do this without running code
   ;;TODO: need some way to write static methods for function classes
   (if (:vars machine)
     (do
       (when-not (contains? @(:vars machine) var)
         (swap! (:vars machine) assoc var (name (gensym (.sym var))))
         (.visitField class-writer
                      (+ (op ACC_PUBLIC)
                         (op ACC_STATIC))
                      (@(:vars machine) var)
                      (.getDescriptor (Type/getType clojure.lang.Var))
                      nil
                      nil)
         (doto (:static-block machine)
           (.visitLdcInsn (name (.getName (.ns var))))
           (.visitLdcInsn (name (.sym var)))
           (load-var)
           (.visitFieldInsn
            (op PUTSTATIC)
            (.getInternalName (:class machine))
            (@(:vars machine) var)
            (.getDescriptor (Type/getType clojure.lang.Var)))))
       (.visitFieldInsn method-writer
                        (op GETSTATIC)
                        (.getInternalName (:class machine))
                        (@(:vars machine) var)
                        (.getDescriptor (Type/getType clojure.lang.Var))))
     (doto method-writer
       (.visitLdcInsn (name (.getName (.ns var))))
       (.visitLdcInsn (name (.sym var)))
       (load-var)))
   (doto method-writer
     (method-call
      (op INVOKEINTERFACE) :clojure.lang.IDeref/deref [:obj]))
   (if (:fn-call machine)
     (do
       (.visitTypeInsn method-writer (op CHECKCAST) "clojure/lang/IFn")
       (update-in machine [:variable-stack] conj (:fn types)))
     (update-in machine [:variable-stack] conj (:obj types))))

  (access-local
   [machine local]
   (info (format "Access local: %s" local))
   ;;access a local value
   (let [[idx _] (first (filter #(= local (second %))
                                (map-indexed vector (:locals machine))))]
     (.visitIntInsn method-writer (op ALOAD) (inc idx))
     (update-in machine [:variable-stack] conj (:obj types))))

  (immediate
   [machine value attrs]
   (info (format "Immediate: %s" value))
   ;;load a literal value
   (let [machine (cond
                  (instance? Integer value)
                  (do
                    (.visitLdcInsn method-writer value)
                    (doto method-writer
                      (.visitTypeInsn (op NEW) "java/lang/Integer")
                      (.visitInsn (op DUP_X1))
                      (.visitInsn (op SWAP))
                      (.visitMethodInsn
                       (op INVOKESPECIAL)
                       "java/lang/Integer"
                       "<init>"
                       (method-desciptor [Type/INT_TYPE (:void types)])))
                    (update-in machine [:variable-stack] conj
                               (type-of machine :int)))
                  (instance? String value)
                  (do
                    (.visitLdcInsn method-writer value)
                    (update-in machine [:variable-stack] conj (:str types)))
                  (symbol? value)
                  (do
                    (if (namespace value)
                      (.visitLdcInsn method-writer (namespace value))
                      (.visitInsn method-writer (op ACONST_NULL)))
                    (doto method-writer
                      (.visitLdcInsn (name value))
                      (method-call (op INVOKESTATIC)
                                   :clojure.lang.Symbol/intern
                                   [:str :str :symbol]))
                    (update-in machine [:variable-stack]
                               conj (type-of machine :symbol))))]
     (if (:do machine)
       (do
         (.visitInsn method-writer (op POP))
         (update-in machine [:variable-stack] pop))
       machine)))

  (define-function [machine expr]
    (info (format "Define function: %s" expr))
    (let [[_ [args & body]] expr]
      (let [class-name (format "%s$%s"
                               (:namespace machine (.getName *ns*))
                               (gensym 'fn))
            class-description (.replaceAll class-name "\\." "/")
            new-machine (doto (assoc machine
                                :fn-call false
                                :do false
                                :class-writer
                                (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                                :vars (atom {})
                                :class (Type/getType
                                        (format "L%s;"
                                                (.replaceAll class-name
                                                             "\\."
                                                             "/"))))
                          (init {:name class-description
                                 :super "clojure/lang/AFn"}))
            new-machine (assoc new-machine
                          :locals args
                          :jaros (:jaros machine)
                          :baos (:baoas machine)
                          :in-function true
                          :static-block (.visitMethod
                                         (:class-writer new-machine)
                                         (op ACC_STATIC)
                                         "<clinit>"
                                         (method-desciptor
                                          [(type-of machine :void)])
                                         nil
                                         nil))
            new-machine (start-procedure new-machine "invoke"
                                         {:method-descriptor
                                          (vec
                                           (repeat (inc (count args))
                                                   (:obj types)))
                                          :access (op ACC_PUBLIC)})

            new-machine (generate-code (cons 'do body) new-machine)
            _ (.visitInsn (:method-writer new-machine) (op ARETURN))
            new-machine (end-procedure new-machine)
            file-name (format "%s.class" class-description)]
        (when-not (:ctor machine) (default-ctor new-machine))
        (.visitInsn (:static-block new-machine) (op RETURN))
        (.visitMaxs (:static-block new-machine) 0 0)
        (.visitEnd (:static-block new-machine))
        (.visitEnd (:class-writer new-machine))
        (.putNextEntry (:jaros machine) (ZipEntry. file-name))
        (copy (.toByteArray (:class-writer new-machine))
              (:jaros machine))
        (.closeEntry (:jaros machine))
        (doto method-writer
          (.visitTypeInsn (op NEW) class-description)
          (.visitInsn (op DUP))
          (method-call (op INVOKESPECIAL)
                       (keyword class-name "<init>")
                       [:void]))
        (update-in machine [:variable-stack] conj (type-of machine :fn)))))

  (fin
   [machine values]
   (info "Fin")
   (when-not (:ctor machine) (default-ctor machine))
   (.visitEnd class-writer)
   (doseq [[a-name [class-writer method-writer]] @(:namespaces machine)]
     (.visitInsn method-writer (op RETURN))
     (.visitMaxs method-writer 0 0)
     (.visitEnd method-writer)
     (.visitEnd class-writer)
     (.putNextEntry (:jaros machine)
                    (ZipEntry. (format "%s__init.class"
                                       (.replaceAll
                                        (name a-name) "\\."  "/"))))
     (copy (.toByteArray class-writer)
           (:jaros machine))
     (.closeEntry (:jaros machine)))
   machine)

  (start-namespace
   [machine namespace]
   (info (format "Namespace: %s" namespace))
   (when-not (contains? @namespaces namespace)
     (let [class-writer (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                          (.visit (op V1_5)
                                  (+ (op ACC_PUBLIC)
                                     (op ACC_FINAL))
                                  (format "%s__init"
                                          (.replaceAll (name namespace)
                                                       "\\." "/"))
                                  nil
                                  "java/lang/Object"
                                  nil))
           method-writer (.visitMethod class-writer
                                       (op ACC_STATIC)
                                       "<clinit>"
                                       (method-desciptor
                                        [(type-of machine :void)])
                                       nil
                                       nil)]
       (.visitField class-writer
                    (+ (op ACC_PUBLIC)
                       (op ACC_STATIC))
                    "namespace"
                    (.getDescriptor (Type/getType clojure.lang.Namespace))
                    nil
                    nil)
       (.visitInsn method-writer (op ACONST_NULL))
       ;;TODO: static field with namespace object in it
       ;;TODO: push/pop bindings for *ns*
       (doto method-writer
         (.visitLdcInsn (name namespace))
         (method-call (op INVOKESTATIC)
                      :clojure.lang.Symbol/intern
                      [:str :symbol])
         (method-call (op INVOKESTATIC)
                      :clojure.lang.Namespace/findOrCreate
                      [:symbol :namespace]))
       (.visitFieldInsn method-writer
                        (op PUTSTATIC)
                        (.getInternalName
                         (Type/getType (format "L%s__init;"
                                               (.replaceAll (name namespace)
                                                            "\\." "/"))))
                        "namespace"
                        (.getDescriptor (Type/getType clojure.lang.Namespace)))
       (swap! namespaces assoc namespace [class-writer method-writer])))
   (assoc machine
     :foo :bar
     :namespace namespace
     :class (Type/getType (format "L%s__init;"
                                  (.replaceAll (name namespace)
                                               "\\." "/")))
     :class-writer (first (get @namespaces namespace))
     :method-writer (second (get @namespaces namespace))))

  (define [machine a-name value]
    (info (format "define: %s %s" a-name value))
    (let [ns (:namespace machine)
          [cw mw] (ns @namespaces)
          new-machine (assoc machine
                        :class-writer cw
                        :method-writer mw
                        :variable-stack nil)
          _ (doto mw
              (.visitLdcInsn (name ns))
              (.visitLdcInsn (name a-name)))
          new-machine (generate-code value new-machine)]
      (doto mw
        (method-call (op INVOKESTATIC)
                     :clojure.lang.RT/var
                     [:str :str :obj :var]))
      (doto method-writer
        (.visitLdcInsn (name ns))
        (.visitLdcInsn (name a-name))
        (load-var))
      (clojure.lang.RT/var (name ns) (name a-name) (PlaceHolder. value))
      (update-in machine [:variable-stack] conj (type-of machine :var)))))

(defn jvm []
  (JVM. types (ClassWriter. ClassWriter/COMPUTE_FRAMES) nil (atom {})))

(defn g []
  (info "Starting compiling")
  ;;CLASSWRITER should be created by the namespace
  (let [class-name "Archimedes/one"
        machine (jvm)
        machine (init machine {:name class-name :super "clojure/lang/AFn"})
        machine (start-procedure machine "invoke"
                                 {:method-descriptor [(type-of machine :obj)]
                                  :access (op ACC_PUBLIC)})
        machine (generate-code '(do (in-ns 'a.b)
                                    (def f (fn* ([x] (println x))))
                                    (f 1))
                               machine)
        #_(generate-code '(do (in-ns 'a.b)
                              (println ((fn* ([x] (+ x x))) 2)))
                         machine)
        #_(generate-code '((fn* ([x] (+ 1 2) x)) "foo") machine)
        machine (end-procedure machine)]
    (fin machine nil)
    (.putNextEntry (:jaros machine)
                   (ZipEntry. (format "%s.class" class-name)))
    (copy (.toByteArray (:class-writer machine))
          (:jaros machine))
    (.closeEntry (:jaros machine))
    (.close (:jaros machine))
    (copy (.toByteArray (:baos machine))
          (file "/tmp/a.jar"))
    (.toByteArray (:baos machine))))
