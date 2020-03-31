;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.utils
  (:require [uncomplicate.commons.core :refer [Mappable mmap unmap]])
  (:import java.security.SecureRandom
           java.io.RandomAccessFile
           java.net.URI
           [java.nio ByteBuffer DirectByteBuffer ByteOrder ShortBuffer IntBuffer LongBuffer]
           [java.nio.file Path Paths Files FileVisitOption OpenOption StandardOpenOption]
           java.nio.file.attribute.FileAttribute
           [java.nio.channels FileChannel FileChannel$MapMode]))

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
  "Evaluates `form` if `status` is zero (e.g. `CL_SUCCESS`), otherwise throws
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
  (or (kw-map kw)
      (dragan-says-ex (format "keyword %s is not applicable." kw) {:applicable (keys kw-map)})))

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

(defn create-temp-dir [prefix]
  (java.nio.file.Files/createTempDirectory (or prefix "uncomplicate_") (make-array FileAttribute 0)))

(defn delete [path]
  (let [options (make-array FileVisitOption 0)]
    (doseq [path (reverse (iterator-seq (.iterator (Files/walk path options))))]
      (Files/deleteIfExists path))))

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

(defn capacity ^long [^ByteBuffer b]
  (.capacity b))

(defn put-float [^ByteBuffer b ^long i ^double x]
  (.putFloat b (* Float/BYTES i) x))

(defn get-float ^double [^ByteBuffer b ^long i]
  (.getFloat b (* Float/BYTES i)))

(defn put-double [^ByteBuffer b ^long i ^double x]
  (.putDouble b (* Double/BYTES i) x))

(defn get-double ^double [^ByteBuffer b ^long i]
  (.getDouble b (* Double/BYTES i)))

(defn put-long [^ByteBuffer b ^long i ^long x]
  (.putLong b (* Long/BYTES i) x))

(defn get-long ^long [^ByteBuffer b ^long i]
  (.getLong b (* Long/BYTES i)))

(defn put-int [^ByteBuffer b ^long i ^long x]
  (.putInt b (* Integer/BYTES i) x))

(defn get-int ^long [^ByteBuffer b ^long i]
  (.getInt b (* Integer/BYTES i)))

(defn put-short [^ByteBuffer b ^long i ^long x]
  (.putShort b (* Short/BYTES i) x))

(defn get-short ^long [^ByteBuffer b ^long i]
  (long (.getShort b (* Short/BYTES i))))

(defn put-short [^ByteBuffer b ^long i ^long x]
  (.put b i x))

(defn get-short ^long [^ByteBuffer b ^long i]
  (long (.get b i)))

(defn mapped-buffer
  ([^FileChannel channel ^long offset ^long size flag]
   (doto (.map channel
               (case flag
                 :read-write FileChannel$MapMode/READ_WRITE
                 :read FileChannel$MapMode/READ_ONLY
                 :read-only FileChannel$MapMode/READ_ONLY
                 :private FileChannel$MapMode/PRIVATE
                 :copy-on-write FileChannel$MapMode/PRIVATE
                 (dragan-says-ex "The requested flag is not supported by java.nio.FileChannel." {:flag flag}))
               (max 0 offset) size)
     (.order ^ByteBuffer (ByteOrder/nativeOrder))))
  ([^FileChannel channel ^long size flag]
   (mapped-buffer channel 0 size flag))
  ([^FileChannel channel size-or-flag]
   (if (keyword? size-or-flag)
     (mapped-buffer channel 0 (.size channel) size-or-flag)
     (mapped-buffer channel 0 size-or-flag :read-write)))
  ([^FileChannel channel]
   (mapped-buffer channel 0 (.size channel) :read-write)))

(defn reverse-short-bytes [^ByteBuffer buffer!]
  (let [b (.asShortBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Short/reverseBytes (.get b i))))
    buffer!))

(defn reverse-int-bytes [^ByteBuffer buffer!]
  (let [b (.asIntBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Integer/reverseBytes (.get b i))))
    buffer!))

(defn reverse-long-bytes [^ByteBuffer buffer!]
  (let [b (.asLongBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Long/reverseBytes (.get b i))))
    buffer!))

(defn dec-open-option [option]
  (case option
    :read StandardOpenOption/READ
    :write StandardOpenOption/WRITE
    :append StandardOpenOption/APPEND
    :truncate StandardOpenOption/TRUNCATE_EXISTING
    :truncate-existing StandardOpenOption/TRUNCATE_EXISTING
    :create StandardOpenOption/CREATE
    :create-new StandardOpenOption/CREATE_NEW
    :delete-on-close StandardOpenOption/DELETE_ON_CLOSE
    :sparse StandardOpenOption/SPARSE
    :sync StandardOpenOption/SYNC
    :dsync StandardOpenOption/DSYNC
    (dragan-says-ex "The requested option is not supported by java's StandardOpenOption." {:option option})))

(defn channel
  ([^RandomAccessFile file]
   (.getChannel file))
  ([p options]
   (cond
     (instance? Path p)
     (FileChannel/open p (into-array OpenOption (map dec-open-option options)))
     (instance? URI p) (channel (Paths/get p) options)
     :other (channel (Paths/get (URI. p)) options))))

(defn random-access
  ([^String path flag]
   (RandomAccessFile. path (case flag
                             :read-write "rw"
                             :read "r"
                             (name flag))))
  ([^String path]
   (RandomAccessFile. path "rw")))

;;====================== RNG Utils ===============================================

(let [srng (SecureRandom.)]
  (defn generate-secure-seed ^long []
    (.getLong (ByteBuffer/wrap (.generateSeed srng Long/BYTES)) 0)))

(defn generate-seed ^long []
  (- (long (rand Long/MAX_VALUE)) (quot Long/MAX_VALUE 2)))

;; ===================== Work Groups utilities ===================================

(defn count-groups ^long [^long per-group ^long items]
  (if (< per-group items)
    (quot (+ items (dec per-group)) per-group)
    1))
