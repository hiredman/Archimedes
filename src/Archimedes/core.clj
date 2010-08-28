(ns Archimedes.core
  (:use [clojure.java.io :only [file copy]])
  (:refer-clojure :exclude [type])
  (:import [clojure.asm ClassWriter Opcodes Type]))

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

(defn int-value [mv]
  (.visitMethodInsn
   mv
   (op INVOKEVIRTUAL)
   (.getInternalName (Type/getType "Ljava/lang/Number;"))
   "intValue"
   (method-desciptor [(type int)])))

(defn long-value [mv]
  (.visitMethodInsn
   mv
   (op INVOKEVIRTUAL)
   (.getInternalName (Type/getType "Ljava/lang/Number;"))
   "longValue"
   (method-desciptor [(type long)])))

(defn double-value [mv]
  (.visitMethodInsn
   mv
   (op INVOKEVIRTUAL)
   (.getInternalName (Type/getType "Ljava/lang/Number;"))
   "doubleValue"
   (method-desciptor [(type long)])))

(defn Aop [cw meth types]
  (.visitMethodInsn cw
                    (op INVOKESTATIC)
                    (.getInternalName (Type/getType "LArchimedes/Ops;"))
                    (name meth)
                    (method-desciptor types)))

(defn box [mv prim box]
  (.visitTypeInsn mv (op NEW) (.getInternalName box))
  (if (< 1 (.getSize prim))
    (do
      (.visitInsn mv (op DUP_X2))
      (.visitInsn mv (op POP)))
    (do
      (.visitInsn mv (op DUP_X1))
      (.visitInsn mv (op SWAP))))
  (.visitMethodInsn mv
                    (op INVOKESPECIAL)
                    (.getInternalName box) "<init>"
                    (method-desciptor [prim (type VOID)])))

(defmulti add (fn [cw & types] (vec types)))

(def o
  {:int {:return (op IRETURN)
         :load (op ILOAD)
         :add (op IADD)
         :type (type int)
         :boxed (Type/getType "Ljava/lang/Integer;")}
   :long {:return (op LRETURN)
          :load (op LLOAD)
          :add (op LADD)
          :type (type long)
          :boxed (Type/getType "Ljava/lang/Long;")}
   :double {:return (op DRETURN)
            :load (op DLOAD)
            :add (op DADD)
            :type (type double)
            :boxed (Type/getType "Ljava/lang/Double;")}
   :float  {:return (op FRETURN)
            :load (op FLOAD)
            :add (op FADD)
            :type (type float)
            :boxed (Type/getType "Ljava/lang/Float;")}
   :java.math.BigInteger
   {:return (op ARETURN)
    :load (op ALOAD)
    :add (fn [mv]
           (let [t (Type/getType "Ljava/math/BigInteger;")]
             (.visitMethodInsn
              mv
              (op INVOKEVIRTUAL)
              (.getInternalName t)
              "add"
              (method-desciptor [t t]))))
    :type (Type/getType "Ljava/math/BigInteger;")}
   :java.math.BigDecimal
   {:return (op ARETURN)
    :load (op ALOAD)
    :add (fn [mv]
           (let [t (Type/getType "Ljava/math/BigDecimal;")]
             (.visitMethodInsn
              mv
              (op INVOKEVIRTUAL)
              (.getInternalName t)
              "add"
              (method-desciptor [t t]))))
    :type (Type/getType "Ljava/math/BigDecimal;")}
   :java.lang.Integer
   {:return (op ARETURN)
    :load (op ALOAD)
    :add (fn [mv]
           (let [t (Type/getType "Ljava/lang/Integer;")]
             (doto mv
               int-value
               (.visitInsn (op SWAP))
               int-value
               (Aop :add [(type int) (type int) (type int)])
               (box (type int) t))))
    :type (Type/getType "Ljava/lang/Integer;")}
   :java.lang.Long
   {:return (op ARETURN)
    :load (op ALOAD)
    :add (fn [mv]
           (let [t (Type/getType "Ljava/lang/Long;")]
             (doto mv
               long-value
               (.visitInsn (op DUP2_X1))
               (.visitInsn (op POP2))
               long-value
               (Aop :add [(type long) (type long) (type long)])
               (box (type long) t))))
    :type (Type/getType "Ljava/lang/Long;")}
   :java.lang.Double
   {:return (op ARETURN)
    :load (op ALOAD)
    :add (fn [mv]
           (let [t (Type/getType "Ljava/lang/Double;")]
             (doto mv
               double-value
               (.visitInsn (op DUP2_X1))
               (.visitInsn (op POP2))
               double-value
               (Aop :add [(type double) (type double) (type double)])
               (box (type double) t))))
    :type (Type/getType "Ljava/lang/Double;")}})

(defn add-fn [cw types]
  (let [[type1 type2 type3] types
        mv (method cw :add (map (comp :type o) types))
        add (-> o type1 :add)
        add (if (fn? add) add (fn [mv] (.visitInsn mv add)))]
    (doto mv
      (.visitCode)
      (.visitIntInsn (-> o type1 :load) 0)
      (.visitIntInsn (-> o type2 :load) (-> o type1 :type .getSize))
      add
      (.visitInsn (-> o type1 :return))
      (.visitMaxs 0 0)
      (.visitEnd))))

(defmacro add! []
  (let [ms (for [[ktype] o]
             `(defmethod ~'add ~(vec (repeat 3 ktype)) [cw# & types#]
                (add-fn cw# types#)))]
    `(do
       ~@ms)))

(add!)

(defn overload [cv fn ptypes & types]
  (let [n (count ptypes)]
    (doseq [t types]
      (apply add cv (repeat n t))))
  cv)

(when *compile-files*
  (let [name "Archimedes/Ops"
        bytes (.toByteArray
               (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                 (header name)
                 (overload add [:a :a :a]
                           :int :long :double :float
                           :java.math.BigInteger :java.math.BigDecimal
                           :java.lang.Integer :java.lang.Long
                           :java.lang.Double)
                 .visitEnd))]
    (.mkdirs (file *compile-path* "Archimedes"))
    (copy bytes (file *compile-path* "Archimedes/Ops.class"))))
