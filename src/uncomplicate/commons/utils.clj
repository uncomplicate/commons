;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.utils
  (:import java.nio.file.Paths
           [java.nio ByteBuffer DirectByteBuffer ByteOrder]))

;; ========= Bitfild masks ========================================

(defn mask
  "Converts keywords to a bitfield mask.

  Given one or more keyword `flag`s, creates a long bitmask
  that can be consumed by native functions. Needs a hashmap `table` that
  contains possible long mappings, 0-2 keywords, and a (possibly empty)
  list of additional keywords.
  If called with `nil` table or an unknown keyword, throws `Illegalargumentexception`.
  The inverse function is [[unmask]].

  Examples:

      (mask {:a 1 :b 2 :c 4} [:a :c]) => 5
      (mask {:a 1 :b 2 :c 4} :a [:c]) => 5
      (mask {:a 1 :b 2 :c 4} :a :c []) => 5
  "
  (^long [table flag1 flag2 flags]
         (apply bit-or (table flag1) (table flag2) (map table flags)))
  (^long [table flag flags]
         (apply bit-or 0 (table flag) (map table flags)))
  (^long [table flags]
         (apply bit-or 0 0 (map table flags))))

(defn unmask
  "Converts a bitfield `mask` to keywords.

  Given a mapping `table` and a bitfield `mask`, returns a lazy sequence
  with decoded keyword flags contained in the bitmask.
  The reverse function is [[mask]].

  Examples:

      (unmask {:a 1 :b 2 :c 4} 5) =>  '(:a :c)
  "
  [table ^long mask]
  (filter identity
          (map (fn [[k v]]
                 (if (= 0 (bit-and mask ^long v))
                   nil
                   k))
               table)))

(defn unmask1
  "Converts a bitfield `mask` to one keyword.

  Given a mapping `table` and a bitfield `mask`, returns the first decoded keyword
  that is contained in the bitmask. This is useful when we know that just
  one of the values in the table fits the bitmask, so the result of [[unmask]]
  would contain one element anyway.
  The reverse function is [[mask]].

  Examples:

      (unmask1 {:a 1 :b 2 :c 4} 2) => :b
  "
  [table ^long mask]
  (some identity
        (map (fn [[k v]]
               (if (= 0 (bit-and mask ^long v))
                   nil
                   k))
             table)))

;; ====================== Error checking ==========================================

(defmacro with-check
  "Evaluates `form` if `status` is not zero (e.g. `CL_SUCCESS`), otherwise throws
  an appropriate `ExceptionInfo` with decoded informative details.
  It helps with native interop methods that return error codes directly, while
  returning computation results through side-effects in arguments.

  Example:

      (with-check (some-call-that-returns-error-code) result)
  "
  [error-fn status form]
  `(let [status# ~status]
     (if (= 0 status#)
       ~form
       (throw (~error-fn status# ~(pr-str form))))))

;; ======================= Messages ===============================================

(defn dragan-says-ex
  ([message data]
   (throw (ex-info (format "Dragan says: %s" message) data)))
  ([message]
   (dragan-says-ex message {})))

;; ======================= Keyword options encoding =================================

(defn enc-keyword [kw-map kw]
  (kw-map kw (dragan-says-ex (format "keyword %s is not applicable." kw) {:available (keys kw-map)})))

;; ======================= Conditional into =======================================

(defn cond-into
  ([x & s]
   (into x (for [[c e] (partition 2 s) :when c]
             e))))

;; ======================= Path Helpers ==========================================

(defn path [^String path-name ^String file-name & file-names]
  (let [file-array (if file-names
                     (into-array String (cons file-name file-names))
                     (let [res (make-array String 1)]
                       (aset ^"[Ljava.lang.String;" res 0 file-name)
                       res))]
    (Paths/get path-name file-array)))

;; ======================= Buffer utils ==========================================

(defn direct-buffer [^long size]
  (let [buff (ByteBuffer/allocateDirect size)]
    (.order ^ByteBuffer buff (ByteOrder/nativeOrder))
    buff))

(defn slice-buffer [^ByteBuffer buf ^long ofst ^long len]
  (when buf
    (let [ord (.order buf)
          res (.duplicate buf)]
      (.position res ofst)
      (.limit res (+ ofst len))
      (.slice res)
      (.order res)
      res)))

(defn put-float [^ByteBuffer b ^long i ^double x]
  (.putFloat b (* Float/BYTES i) x))

(defn get-float [^ByteBuffer b ^long i]
  (.getFloat b (* Float/BYTES i)))

(defn put-double [^ByteBuffer b ^long i ^double x]
  (.putDouble b (* Double/BYTES i) x))

(defn get-double [^ByteBuffer b ^long i]
  (.getDouble b (* Double/BYTES i)))

(defn put-long [^ByteBuffer b ^long i ^long x]
  (.putLong b (* Long/BYTES i) x))

(defn get-long [^ByteBuffer b ^long i]
  (.getLong b (* Long/BYTES i)))

(defn put-int [^ByteBuffer b ^long i ^long x]
  (.putInt b (* Integer/BYTES i) x))

(defn get-int [^ByteBuffer b ^long i]
  (.getInt b (* Integer/BYTES i)))
