(ns Archimedes.types
  (:refer-clojure :exclude [type])
  (:use [Archimedes.asm])
  (:import [clojure.asm Type Label]))

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
   (op INVOKESTATIC)
   "LArchimedes/Ops;"
   (name nam)
   (method-desciptor
    (map asmType types))))

(defn numOps [mv nam types]
  (.visitMethodInsn
   mv
   (op INVOKEVIRTUAL)
   "Ljava/lang/Number;"
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
   (.visitInsn mv (op DUP_X2))
   (.visitInsn mv (op POP))
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
   (numOps mv :longValue [(unboxedType t)]))
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
   [n mv]
   (binary-boxed-long n mv :remainder)
   n)
  (Nequiv [n cv]
          n)
  (lt [n cv]
      n)
  (negate
   [n cv]
   (unbox n cv)
   (Aop cv :negate [(unboxedType n)])
   (box (unboxedType n) cv)
   n)
  (Ninc
   [n cv]
   (.visitLdcInsn cv (Long. "1"))
   (box (unboxedType n) cv)
   (Aop cv :add [n])
   n)
  (Ndec
   [n cv]
   (unbox n cv)
   (.visitLdcInsn cv (Long. "1"))
   (negate (unboxedType n) cv)
   (Aop cv :add [(unboxedType n)])
   (box (unboxedType n) cv)
   n))

(defn boxed-long []
  (BLong.))

(defn instance-jmp [mv atype label]
  (doto mv
    (.visitInsn (op DUP))
    (.visitTypeInsn (op INSTANCEOF) (.getInternalName (asmType atype)))
    (.visitJumpInsn (op IFNE) label)))

(defmacro labels [labels & body]
  (let [labs (vec
              (mapcat
               (fn [[nam]]
                 (let [name (symbol (.toUpperCase (name nam)))]
                   [name `(new Label)]))
                   labels))]
    `(let ~labs
       (letfn ~labels
         ~@body))))

(defrecord INumber []
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
   (Type/getType "Ljava/lang/Number;"))
  ANumber
  (isZero [n mv]
          n)
  (isPos [n mv]
         n)
  (isNeg [n mv]
         n)
  (add
   [n mv]
   (let [LONG-NUMBER (Label.)
         LONG-LONG (Label.)
         START (Label.)
         END (Label.)
         ERROR (Label.)]
     (letfn [(error
              [mv]
              (.visitInsn mv (op POP))
              (.visitInsn mv (op POP))
              (.visitLdcInsn mv (Long. "1"))
              (.visitInsn mv (op LNEG))
              (box (unboxedType (BLong.))  mv)
              (.visitJumpInsn mv (op GOTO) END))
             (long-number
              [mv]
              (.visitInsn mv (op SWAP))
              (instance-jmp mv (BLong.) LONG-LONG)
              (.visitJumpInsn mv (op GOTO) ERROR))
             (long-long
              [mv]
              (doto mv
                (.visitTypeInsn (op CHECKCAST) (.getInternalName
                                                (asmType (BLong.))))
                (.visitInsn (op SWAP))
                (.visitTypeInsn (op CHECKCAST) (.getInternalName
                                                (asmType (BLong.))))
                (Aop :add (repeat 3 (BLong.)))
                (.visitJumpInsn (op GOTO) END)))]
       (doto mv
         (instance-jmp (BLong.) LONG-NUMBER)
         (.visitJumpInsn (op GOTO) ERROR)

         (.visitLabel LONG-NUMBER)
         long-number

         (.visitLabel LONG-LONG)
         long-long

         (.visitLabel ERROR)
         error

         (.visitLabel END))))
   n)
  (multiply
   [n mv]
   n)
  (divide
   [n mv]
   n)
  (quotient [n cv]
            n)
  (remainder
   [n mv]
   n)
  (Nequiv [n cv]
          n)
  (lt [n cv]
      n)
  (negate
   [n cv]
   n)
  (Ninc
   [n cv]
   n)
  (Ndec
   [n cv]
   n))

(defn method-list []
  (for [[nam {doc :doc}] (:sigs ANumber)]
    [nam @(ns-resolve *ns* (symbol (name nam)))
     (read-string doc)]))

(def n *ns*)

(defn types []
  (->> (ns-imports n)
       vals
       (map (memfn getName))
       (filter #(.startsWith % "Archimedes.types"))
       (map #(Class/forName %))
       (map (memfn newInstance))))
