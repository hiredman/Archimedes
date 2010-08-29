(ns Archimedes.types
  (:refer-clojure :exclude [type])
  (:use [Archimedes.asm])
  (:import [clojure.asm Type Label]))

(defprotocol ANumber
  (isZero [T method-visitor class-visitor] "1")
  (isPos [T method-visitor class-visitor] "1")
  (isNeg [T method-visitor class-visitor] "1")
  (add [T method-visitor class-visitor] "2")
  (multiply [T method-visitor class-visitor] "2")
  (divide [T method-visitor class-visitor] "2")
  (quotient [T method-visitor class-visitor] "2")
  (remainder [T method-visitor class-visitor] "2")
  (Nequiv [T method-visitor class-visitor] "2")
  (lt [T method-visitor class-visitor] "2")
  (negate [T method-visitor class-visitor] "1")
  (Ninc [T method-visitor class-visitor] "1")
  (Ndec [T method-visitor class-visitor] "1"))

(defprotocol VMType
  (return [T method-visitor])
  (Tload [T index method-visitor])
  (asmType [T])
  (Tsize [T])
  (ifeq [T method-visitor label]))

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

(defn boxed-bridge [class-visitor Ts name]
  (let [[T] Ts
        method-visitor (method class-visitor name
                               (conj (vec (map boxedType (butlast Ts)))
                                     (asmType (last Ts))))
        unbox (fn [mv] (unbox T mv))]
    (dotimes [i (dec (count Ts))]
      (.visitIntInsn method-visitor (op ALOAD) i))
    (case (dec (count Ts))
          1 (unbox method-visitor)
          2 (doto method-visitor
              unbox
              (.visitInsn (op SWAP))
              unbox)
          3 (doto method-visitor
              unbox
              (.visitInsn (op DUP_X2))
              (.visitInsn (op POP))
              unbox
              (.visitInsn (op SWAP))
              unbox))
    (doto method-visitor
      (Aop name Ts)
      ((fn [mv]
         (return (last Ts) mv)))
      (.visitMaxs 0 0)
      (.visitEnd))))

(defn make-method [Ts class-visitor name op]
  (let [method-visitor (method
                        class-visitor name
                        (map asmType Ts))
        [T] Ts]
    (dotimes [i (dec (count Ts))]
      (Tload T (* i (Tsize T)) method-visitor))
    (op method-visitor)
    (return (last Ts) method-visitor)
    (.visitMaxs method-visitor 0 0)
    (.visitEnd method-visitor))
  (boxed-bridge class-visitor Ts name))

(defrecord Bool []
  VMType
  (return
   [T method-visitor]
   (.visitInsn method-visitor (op IRETURN)))
  (Tload
   [T index method-visitor]
   (.visitIntInsn method-visitor (op ILOAD) index)
   T)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (type boolean)))

(defn is-zero? [c T]
  (fn [method-visitor]
    (labels
     [(is
       [mw]
       (ldc mw true)
       (goto mw END))
      (is-not
       [mw]
       (ldc mw false)
       (goto mw END))
      (end [mw])]
     (doto method-visitor
       (ldc c)
       ((fn [method-visitor]
          (ifeq T method-visitor IS)))
       (goto IS-NOT)
       (label IS)
       is
       (label IS-NOT)
       is-not
       (label END)))))

(defrecord Int []
  VMType
  (return
   [T method-visitor]
   (.visitInsn method-visitor (op IRETURN)))
  (Tload
   [T index method-visitor]
   (.visitIntInsn method-visitor (op ILOAD) index)
   T)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (type int))
  (ifeq
   [T method-visitor label]
   (doto method-visitor
     (.visitJumpInsn (op IF_ICMPEQ) label)))
  Boxable
  (box
   [t mv]
   (.visitTypeInsn mv (op NEW) (.getInternalName (boxedType t)))
   (.visitInsn mv (op DUP_X1))
   (.visitInsn mv (op SWAP))
   (.visitMethodInsn mv (op INVOKESPECIAL) (.getInternalName (boxedType t))
                     "<init>"
                     (method-desciptor [(asmType t) (type VOID)])))
  Boxed
  (unbox
   [T method-visitor]
   (numOps method-visitor :intValue [T]))
  (boxedType
   [t]
   (Type/getType "Ljava/lang/Integer;"))
  ANumber
  (isZero
   [T method-visitor class-visitor]
   (make-method
    [T (Bool.)] class-visitor :isZero
    (is-zero?
     (int 0)
     T))
   T)
  (isPos [T method-visitor class-visitor]
         T)
  (isNeg [T method-visitor class-visitor]
         T)
  (add
   [T method-visitor class-visitor]
   (make-method
    [T T T] class-visitor :add
    (fn [method-visitor] (.visitInsn method-visitor (op IADD))))
   T)
  (multiply
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :multiply
                (fn [method-visitor] (.visitInsn method-visitor (op IMUL))))
   T)
  (divide
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :divide
                (fn [method-visitor] (.visitInsn method-visitor (op IDIV))))
   T)
  (quotient
   [T method-visitor class-visitor]
   T)
  (remainder
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :remainder
                (fn [method-visitor]
                  (.visitInsn method-visitor (op IREM))))
   T)
  (Nequiv
   [T method-visitor class-visitor]
   T)
  (lt
   [T method-visitor class-visitor]
   T)
  (negate
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :negate
                (fn [method-visitor]
                  (.visitInsn method-visitor (op INEG))))
   T)
  (Ninc
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :inc
                (fn [method-visitor]
                  (.visitLdcInsn method-visitor 1)
                  (Aop method-visitor :add [T T T])))
   T)
  (Ndec
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :dec
                (fn [method-visitor]
                  (.visitLdcInsn method-visitor (Integer. 1))
                  (Aop method-visitor :negate [T T])
                  (Aop method-visitor ::add [T T T])))
   T))

(defrecord Flt []
  VMType
  (return
   [T method-visitor]
   (.visitInsn method-visitor (op FRETURN)))
  (Tload
   [T index method-visitor]
   (.visitIntInsn method-visitor (op FLOAD) index)
   T)
  (Tsize
   [t]
   (.getSize (asmType t)))
  (asmType
   [t]
   (type float))
  (ifeq
   [T method-visitor label]
   (.visitInsn method-visitor (op FCMPL))
   (.visitJumpInsn method-visitor (op IFEQ) label))
  Boxable
  (box
   [t mv]
   (.visitTypeInsn mv (op NEW) (.getInternalName (boxedType t)))
   (.visitInsn mv (op DUP_X1))
   (.visitInsn mv (op SWAP))
   (.visitMethodInsn mv (op INVOKESPECIAL) (.getInternalName (boxedType t))
                     "<init>"
                     (method-desciptor [(asmType t) (type VOID)])))
  Boxed
  (unbox
   [T method-visitor]
   (numOps method-visitor :floatValue [T]))
  (boxedType
   [t]
   (Type/getType "Ljava/lang/Float;"))
  ANumber
  (isZero
   [T method-visitor class-visitor]
   (make-method
    [T (Bool.)] class-visitor :isZero
    (is-zero? (float 0) T))
   T)
  (isPos [T method-visitor class-visitor]
         T)
  (isNeg [T method-visitor class-visitor]
         T)
  (add
   [T method-visitor class-visitor]
   (make-method
    [T T T] class-visitor :add
    (fn [method-visitor] (.visitInsn method-visitor (op FADD))))
   T)
  (multiply
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :multiply
                (fn [method-visitor] (.visitInsn method-visitor (op FMUL))))
   T)
  (divide
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :divide
                (fn [method-visitor] (.visitInsn method-visitor (op FDIV))))
   T)
  (quotient
   [T method-visitor class-visitor]
   T)
  (remainder
   [T method-visitor class-visitor]
   (make-method [T T T] class-visitor :remainder
                (fn [method-visitor]
                  (.visitInsn method-visitor (op FREM))))
   T)
  (Nequiv
   [T method-visitor class-visitor]
   T)
  (lt
   [T method-visitor class-visitor]
   T)
  (negate
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :negate
                (fn [method-visitor]
                  (.visitInsn method-visitor (op FNEG))))
   T)
  (Ninc
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :inc
                (fn [method-visitor]
                  (.visitLdcInsn method-visitor (float 1))
                  (Aop method-visitor :add [T T T])))
   T)
  (Ndec
   [T method-visitor class-visitor]
   (make-method [T T] class-visitor :dec
                (fn [method-visitor]
                  (.visitLdcInsn method-visitor (float 1))
                  (Aop method-visitor :negate [T T])
                  (Aop method-visitor ::add [T T T])))
   T))

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
       (remove #{"Archimedes.types.Bool"})
       (map #(Class/forName %))
       (map (memfn newInstance))))
