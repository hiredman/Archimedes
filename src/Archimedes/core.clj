(ns Archimedes.core
  (:use [clojure.java.io :only [file copy]]
        [Archimedes.types]
        [Archimedes.asm]
        [clojure.set])
  (:refer-clojure :exclude [type])
  (:import [clojure.asm ClassWriter Opcodes Type]))

;; index-combinations and combinations take from
;; clojure.contrib.combinatorics because that namespace doesn't load

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n))))))
         iter-comb
         (fn iter-comb [c j]
           (if (> j n) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j))))))))
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 (inc n)))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step
                               (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
        (let [cnt (count items)]
          (cond (> n cnt) nil
                (= n cnt) (list (seq items))
                :else
                (map #(map v-items %) (index-combinations n cnt)))))))

(def db (ref #{}))

(defn store [& relations]
  (dosync
   (alter db into relations)))

(defn match [q pos]
  (cond
   (and (vector? q) (vector? pos))
   (every? true? (map match q pos))
   (= q pos)
   true
   (isa? q pos)
   true
   (= q :_)
   true
   :else false))

(defn q [relation]
  (filter
   #(match relation %)
   @db))

(def class-name "Archimedes.Ops")

(derive ::number ::object)

(derive ::exact ::number)
(derive ::fp ::number)
(derive ::wide ::number)
(derive ::bignum ::number)

(def synthetic-types #{::exact ::fp ::wide ::bignum})

(derive ::int ::exact)
(derive ::ratio ::exact)
(derive ::float ::fp)
(derive ::double ::fp)
(derive ::double ::wide)
(derive ::long ::exact)
(derive ::long ::wide)
(derive ::bigint ::bignum)

(declare numbers)

(defn derive-return
  ([T1] T1)
  ([T1 T2]
     (let [rules (q [::rule T1 T2 :_])
           x (reduce intersection (map descendants (map last rules)))]
       (println "types" T1 T2)
       (println "choices" x)
       (if (T1 x)
         T1
         (first x)))))

(store [::object (Type/getType "Ljava/lang/Object;")]
       [::long (Type/getType "Ljava/lang/Long;")]
       [::double (Type/getType "Ljava/lang/Double;")]
       [::op :add 2]
       [::op :dec 1]
       [::op :isZero 1 ::boolean]
       [::rule ::number ::number ::number]
       [::rule ::exact ::exact ::exact]
       [::rule ::exact ::fp ::fp]
       [::rule ::wide ::number ::wide]
       [::rule ::number ::wide ::wide]
       [::rule ::high-precision ::number ::high-precision]
       )

(def numbers
  (->> (descendants ::number)
       (remove synthetic-types)))

(def ops
  (map
   (comp vec rest)
   (q [::op :_ :_])))

(doseq [method (sort-by
                  first
                  (concat
                   (for [[op-name op-arity return-type] ops
                         argument-types (combinations
                                         numbers op-arity)]
                     [op-name (conj (vec argument-types)
                                    (or return-type
                                        (apply derive-return argument-types)))])
                   (for [[op-name op-arity return-type] ops
                         argument-type numbers
                         :let [argument-types (repeat op-arity argument-type)]]
                     [op-name (conj (vec argument-types)
                                    (or return-type
                                        (apply derive-return argument-types)))])
                   #_(for [[op-name op-arity return-type] ops
                           :let [argument-types (repeat
                                                 op-arity
                                                 ::object)]]
                       [op-name (conj (vec argument-types)
                                      (or return-type :object))])))]
    (store [::method method]))

#_(when *compile-files*
    (let [name (.replaceAll class-name "\\." "/")
          bytes (.toByteArray
                 (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                   (header name)
                   ((fn [cv]
                      (doseq [t (types)
                              [name gen argc] (method-list)]
                        (gen t nil2 cv))))
                   .visitEnd))]
      (.mkdirs (file *compile-path* (first (.split name "/"))))
      (copy bytes (file *compile-path* (str name ".class")))))

(when *compile-files*
    (let [name (.replaceAll class-name "\\." "/")
          class-writer (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
                         (header name))
          methods (q [::method :_])]
      (doseq [[_ method] (sort-by first (q [::method :_]))]
        (when-not (some #(some #{::synthetic} (parents %)) (second method))
          (println method)))
      (.visitEnd class-writer)
      (let [bytes (.toByteArray class-writer)]
        (.mkdirs (file *compile-path* (first (.split name "/"))))
        (copy bytes (file *compile-path* (str name ".class"))))))
