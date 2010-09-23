(ns Archimedes.compiler.jvm
  (:use [Archimedes.compiler]
        [clojure.java.io :only [copy file]])
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
   :fn (Type/getType "Lclojure/lang/IFn;")
   :void Type/VOID_TYPE})

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

(defrecord JVM [types class-writer method-writer]
  Machine

  (init
   [machine values]
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
   (name types))

  (start-procedure
   [machine name attrs]
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
   (.visitMaxs method-writer 0 0)
   (.visitEnd method-writer)
   (:previous-machine machine))

  (procedure-call
   [machine a-name args]
   (let [klass (Class/forName (namespace a-name))
         method (name a-name)
         argc (:argc args)]
     (object-array method-writer argc)
     (if (:static args)
       (do
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
         (update-in machine [:variable-stack]
                    (fn [stack]
                      (conj (nth (iterate pop stack) argc)
                            (:obj types))))))))

  (function-call
   [machine args]
   (method-call method-writer
                (op INVOKEINTERFACE)
                :clojure.lang.IFn/invoke
                (repeat (inc args) :obj))
   machine)

  (resolve-var
   [machine var]
   (doto method-writer
     (.visitLdcInsn (name (.getName (.ns var))))
     (.visitLdcInsn (name (.sym var)))
     (method-call
      (op INVOKESTATIC)
      :clojure.lang.RT/var
      [:str :str :var])
     (method-call
      (op INVOKEINTERFACE)
      :clojure.lang.IDeref/deref
      [:obj]))
   (if (:fn-call machine)
     (do
       (.visitTypeInsn method-writer (op CHECKCAST) "clojure/lang/IFn")
       (update-in machine [:variable-stack] conj (:fn types)))
     (update-in machine [:variable-stack] conj (:obj types))))

  (access-local
   [machine local]
   (let [[idx _] (first (filter #(= local (second %))
                                (map-indexed vector (:locals machine))))]
     (.visitIntInsn method-writer (op ALOAD) (inc idx))
     (update-in machine [:variable-stack] conj (:obj types))))

  (immediate
   [machine value attrs]
   (.visitLdcInsn method-writer value)
   (cond
    (instance? Integer value)
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
      (update-in machine [:variable-stack] conj (:int types)))
    (instance? String value)
    (update-in machine [:variable-stack] conj (:str types))))

  (define-function [machine expr]
    (let [[_ [args & body]] expr]
      (let [class-name (format "Archimedes._20$%s" (gensym 'fn))
            class-description (.replaceAll class-name "\\." "/")
            new-machine (doto (assoc machine
                                :class-writer
                                (ClassWriter. ClassWriter/COMPUTE_FRAMES))
                          (init {:name class-description
                                 :super "clojure/lang/AFn"}))
            new-machine (assoc new-machine
                          :locals args
                          :jaros (:jaros machine)
                          :baos (:baoas machine))

            new-machine (start-procedure new-machine "invoke"
                                         {:method-descriptor
                                          (vec
                                           (repeat (inc (count args))
                                                   (:obj types)))
                                          :access (op ACC_PUBLIC)})

            new-machine (generate-code (cons 'do body) new-machine)
            _ (println new-machine)
            _ (.visitInsn (:method-writer new-machine) (op ARETURN))
            new-machine (end-procedure new-machine)
            file-name (format "%s.class" class-description)]
        (fin new-machine nil)
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
        (update-in machine [:variable-stack] conj (:fn types)))))
  (fin
   [machine values]
   (when-not (:ctor machine) (default-ctor machine))
   (.visitEnd class-writer)))

(defn g []
  (let [class-name "Archimedes/_25"
        machine (JVM. types (ClassWriter. ClassWriter/COMPUTE_FRAMES) nil)
        machine (init machine {:name class-name :super "clojure/lang/AFn"})
        machine (start-procedure machine "invoke"
                                 {:method-descriptor [(type-of machine :obj)]
                                  :access (op ACC_PUBLIC)})
        machine (generate-code '((fn* ([x] x)) "foo") machine)
        _ (.visitInsn (:method-writer machine) (op ARETURN))
        machine (end-procedure machine)]
    (fin machine nil)
    (.putNextEntry (:jaros machine)
                   (ZipEntry. (format "%s.class" class-name)))
    (copy (.toByteArray (:class-writer machine))
          (:jaros machine))
    (.closeEntry (:jaros machine))
    (.close (:jaros machine))
    (copy (.toByteArray (:baos machine))
          (file "/tmp/a.jar"))))
