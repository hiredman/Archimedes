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

(defprotocol ANumber
  (isZero [T mv] "1")
  (isPos [T mv] "1")
  (isNeg [T mv] "1")
  (add [T mv] "2")
  (multiply [T mv] "2")
  (divide [T mv] "2")
  (quotient [T mv] "2")
  (remainder [T mv] "2")
  (Nequiv [T mv] "2")
  (lt [T mv] "2")
  (negate [T mv] "1")
  (Ninc [T mv] "1")
  (Ndec [T mv] "1"))

(defprotocol VMType
  (return [t cv])
  (Tload [t n cv])
  (asmType [t])
  (Tsize [t]))

(declare asmType)

(defn Aop [mv nam types]
  (.visitMethodInsn
   mv
   (op INVOKEINTERFACE)
   "LArchimedes/Ops;"
   (name nam)
   (method-desciptor
    (map asmType types))))

(defprotocol Boxed
  (unbox [t cv])
  (unboxedType [t]))

(defprotocol Boxable
  (box [t cv])
  (boxedType [t]))

(defrecord PInteger []
  VMType
  (return
   [t cv]
   (.visitInsn cv (op IRETURN)))
  (Tload
   [t n cv]
   (.visitIntInsn cv (op ILOAD) n)
   t)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (type int))
  Boxable
  (box [t cv])
  (boxedType [t])
  ANumber
  (isZero [n mv]
          n)
  (isPos [n mv]
         n)
  (isNeg [n mv]
         n)
  (add
   [n mv]
   (.visitInsn mv (op IADD))
   n)
  (multiply
   [n mv]
   (.visitInsn mv (op IMUL))
   n)
  (divide
   [n cv]
   (.visitInsn cv (op IDIV))
   n)
  (quotient [n cv]
            n)
  (remainder
   [n cv]
   (.visitInsn cv (op IREM))
   n)
  (Nequiv [n cv]
          n)
  (lt [n cv]
      n)
  (negate
   [n cv]
   (.visitInsn cv (op INEG))
   n)
  (Ninc
   [n cv]
   (.visitLdcInsn cv 1)
   (add n cv)
   n)
  (Ndec
   [n cv]
   (.visitLdcInsn cv 1)
   (negate n cv)
   (add n cv)
   n))

(declare boxed-long)

(defrecord PLong []
  VMType
  (return
   [t cv]
   (.visitInsn cv (op LRETURN)))
  (Tload
   [t n cv]
   (.visitIntInsn cv (op LLOAD) n)
   t)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (type long))
  Boxable
  (box
   [t mv]
   (.visitTypeInsn mv (op NEW) (.getInternalName
                                (asmType (boxedType t))))
   (.visitInsn mv (op DUP_X2))
   (.visitMethodInsn mv (op INVOKESPECIAL)
                     (.getInternalName
                      (asmType (boxedType t)))
                     "<init>"
                     (method-desciptor [(asmType t) (type VOID)])))
  (boxedType
   [t]
   (boxed-long))
  ANumber
  (isZero [n mv]
          n)
  (isPos [n mv]
         n)
  (isNeg [n mv]
         n)
  (add
   [n mv]
   (.visitInsn mv (op LADD))
   n)
  (multiply
   [n mv]
   (.visitInsn mv (op LMUL))
   n)
  (divide
   [n cv]
   (.visitInsn cv (op LDIV))
   n)
  (quotient [n cv]
            n)
  (remainder
   [n cv]
   (.visitInsn cv (op LREM))
   n)
  (Nequiv [n cv]
          n)
  (lt [n cv]
      n)
  (negate
   [n cv]
   (.visitInsn cv (op LNEG))
   n)
  (Ninc
   [n cv]
   (.visitLdcInsn cv (Long. "1"))
   (add n cv)
   n)
  (Ndec
   [n cv]
   (.visitLdcInsn cv (Long. "1"))
   (negate n cv)
   (add n cv)
   n))

(defn binary-boxed-long [n mv name]
  (unbox n mv)
  (.visitInsn mv (op DUP2_X1))
  (.visitInsn mv (op POP2))
  (unbox n mv)
  (Aop mv name (repeat 3 (unboxedType n)))
  (box (unboxedType n) mv))

(defrecord BLong []
  VMType
  (return
   [t cv]
   (.visitInsn cv (op ARETURN)))
  (Tload
   [t n cv]
   (.visitIntInsn cv (op ALOAD) n)
   t)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (Type/getType "Ljava/lang/Long;"))
  Boxed
  (unbox
   [t mv]
   (.visitMethodInsn
    mv
    (op INVOKEINTERFACE)
    "Ljava/lang/Number;"
    "longValue"
    (method-desciptor [(asmType (unboxedType t))])))
  (unboxedType
   [t]
   (PLong.))
  ANumber
  (isZero [n mv]
          n)
  (isPos [n mv]
         n)
  (isNeg [n mv]
         n)
  (add
   [n mv]
   (binary-boxed-long n mv :add)
   n)
  (multiply
   [n mv]
   (binary-boxed-long n mv :multiply)
   n)
  (divide
   [n mv]
   (binary-boxed-long n mv :divide)
   n)
  (quotient [n cv]
            n)
  (remainder
   [n cv]
   (.visitInsn cv (op LREM))
   n)
  (Nequiv [n cv]
          n)
  (lt [n cv]
      n)
  (negate
   [n cv]
   (.visitInsn cv (op LNEG))
   n)
  (Ninc
   [n cv]
   (.visitLdcInsn cv (Long. "1"))
   (box (unboxedType n) cv)
   (add n cv)
   n)
  (Ndec
   [n cv]
   (.visitLdcInsn cv (Long. "1"))
   (negate (unboxedType n) cv)
   (box (unboxedType n) cv)
   (add n cv)
   n))

(defn boxed-long []
  (BLong.))

(defn method-list []
  (for [[nam {doc :doc}] (:sigs ANumber)]
    [nam @(ns-resolve *ns* (symbol (name nam)))
     (read-string doc)]))

(defn types []
  (->> (ns-imports *ns*)
       vals
       (map (memfn getName))
       (filter #(.startsWith % (name (.getName *ns*))))
       (map #(Class/forName %))
       (map (memfn newInstance))))

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
