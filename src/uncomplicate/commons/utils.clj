;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.utils
  "Assorted utility functions used across Uncomplicate projects.

  For converting keyword options to integers, use [[mask]], [[unmask]], and [[unmask1]].

  For converting C-style error as returned numbers to exceptions, use [[with-check]] and
  [[dragan-says-ex]].

  A few assorted functions: [[enc-keyword]], [[cond-into]].

  When working with `ByteBuffers`, enlist the following functions for help:
  [[direct-buffer]], [[slice-buffer]], [[capacity]],
  [[put-float!]] [[get-float]] [[put-double!]] [[get-double]] [[put-long!]] [[get-long]]
  [[put-int!]] [[get-int]] [[put-short!]] [[get-short]] [[put-byte!]] [[get-byte]]

  When mapping files to direct buffers, these functions can help.
  [[mapped-buffer]], [[reverse-short-bytes!]], [[reverse-long-bytes!]]

  The following functions are useful when working with Java file channels.
  [[dev-open-option]], [[channel]] [[random-access]]

  Sometimes we need a pseudo-random number for kick-starting a random number generator.
  [[generate-secure-seed]], [[generate-seed]]

  And we may need to count groups of items in a larger group.
  [[count-groups]]
  "
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
  "Throws and `ex-info` with an explanation and a tip towards a solution."
  ([message data]
   (throw (ex-info (format "Dragan says: %s" message) data)))
  ([message]
   (dragan-says-ex message {})))

;; ======================= Keyword options encoding =================================

(defn enc-keyword
  "Given a hash-map and a key, returns the appropriate value, or throws an exception
  with the listing of available keys."
  [kw-map kw]
  (or (kw-map kw)
      (dragan-says-ex (format "keyword %s is not applicable." kw) {:applicable (keys kw-map)})))

;; ======================= Conditional into =======================================

(defn cond-into
  "Puts all even arguments into collection `x`, given that the previous argument was not nil or false."
  ([x & s]
   (into x (for [[c e] (partition 2 s) :when c]
             e))))

;; ======================= Path Helpers ==========================================

(defn path
  "Builds Java Path from the appropriate path name and file names."
  [^String path-name ^String file-name & file-names]
  (let [file-array (if file-names
                     (into-array String (cons file-name file-names))
                     (let [res (make-array String 1)]
                       (aset ^"[Ljava.lang.String;" res 0 file-name)
                       res))]
    (Paths/get path-name file-array)))

(defn create-temp-dir
  "Creates a temp directory appropriate for the operating system, prefixed with 'uncomplicate_".
  [prefix]
  (java.nio.file.Files/createTempDirectory (or prefix "uncomplicate_") (make-array FileAttribute 0)))

(defn delete
  "Deletes the path, if it exists."
  [path]
  (let [options (make-array FileVisitOption 0)]
    (doseq [path (reverse (iterator-seq (.iterator (Files/walk path options))))]
      (Files/deleteIfExists path))))

;; ======================= Buffer utils ==========================================

(defn direct-buffer
  "Allocates a direct buffer with native byte order."
  [^long size]
  (let [buff (ByteBuffer/allocateDirect size)]
    (.order ^ByteBuffer buff (ByteOrder/nativeOrder))
    buff))

(defn slice-buffer
  "Slices a buffer from offset with the given length."
  [^ByteBuffer buf ^long ofst ^long len]
  (when buf
    (let [ord (.order buf)
          res (.duplicate buf)]
      (.position res ofst)
      (.limit res (+ ofst len))
      (.slice res)
      (.order res)
      res)))

(defn capacity
  "Returns the capacity of a byte buffer."
  ^long [^ByteBuffer b]
  (.capacity b))

(defn put-float!
  "Puts a float into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^double x]
  (.putFloat b (* Float/BYTES i) x))

(defn get-float
  "Gets a float value at position `i` in a byte buffer."
  ^double [^ByteBuffer b ^long i]
  (.getFloat b (* Float/BYTES i)))

(defn put-double!
  "Puts a double into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^double x]
  (.putDouble b (* Double/BYTES i) x))

(defn get-double
  "Gets a double value at position `i` in a byte buffer."
  ^double [^ByteBuffer b ^long i]
  (.getDouble b (* Double/BYTES i)))

(defn put-long!
  "Puts a long into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^long x]
  (.putLong b (* Long/BYTES i) x))

(defn get-long
  "Gets a long value at position `i` in a byte buffer."
  ^long [^ByteBuffer b ^long i]
  (.getLong b (* Long/BYTES i)))

(defn put-int!
  "Puts an int into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^long x]
  (.putInt b (* Integer/BYTES i) x))

(defn get-int
  "Gets an int value at position `i` in a byte buffer."
  ^long [^ByteBuffer b ^long i]
  (.getInt b (* Integer/BYTES i)))

(defn put-short!
  "Puts a short into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^long x]
  (.putShort b (* Short/BYTES i) x))

(defn get-short
  "Gets a short value at position `i` in a byte buffer."
  ^long [^ByteBuffer b ^long i]
  (long (.getShort b (* Short/BYTES i))))

(defn put-byte!
  "Puts a byte into a byte buffer, at position `i`."
  [^ByteBuffer b ^long i ^long x]
  (.put b i (byte x)))

(defn get-byte
  "Gets a byte value at position `i` in a byte buffer."
  ^long [^ByteBuffer b ^long i]
  (long (.get b i)))

(defn mapped-buffer
  "Returns a `ByteBuffer` mapped to a `FileChannel` of given `size`, from `offset`."
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

(defn reverse-short-bytes!
  "Reverses bytes in short numbers in a given `ByteBuffer`"
  [^ByteBuffer buffer!]
  (let [b (.asShortBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Short/reverseBytes (.get b i))))
    buffer!))

(defn reverse-int-bytes!
  "Reverses bytes in integer numbers in a given `ByteBuffer`"
  [^ByteBuffer buffer!]
  (let [b (.asIntBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Integer/reverseBytes (.get b i))))
    buffer!))

(defn reverse-long-bytes!
  "Reverses bytes in long numbers in a given `ByteBuffer`"
  [^ByteBuffer buffer!]
  (let [b (.asLongBuffer buffer!)]
    (dotimes [i (.capacity b)]
      (.put b i (Long/reverseBytes (.get b i))))
    buffer!))

(defn dec-open-option
  "Decode keywords to `StandardOpenOption`'s"
  [option]
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
  "Creates a file channel"
  ([^RandomAccessFile file]
   (.getChannel file))
  ([p options]
   (cond
     (instance? Path p)
     (FileChannel/open p (into-array OpenOption (map dec-open-option options)))
     (instance? URI p) (channel (Paths/get p) options)
     :other (channel (Paths/get (URI. p)) options))))

(defn random-access
  "Creates a random access file."
  ([^String path flag]
   (RandomAccessFile. path (case flag
                             :read-write "rw"
                             :read "r"
                             (name flag))))
  ([^String path]
   (RandomAccessFile. path "rw")))

;;====================== RNG Utils ===============================================

(let [srng (SecureRandom.)]
  (defn generate-secure-seed
    "Generates a secure seed. This pseudo-random number is `random more than enough` for almost all
  purposes Uncomplicate projects deal with, but it is often slow to generate as we have to wait for
  the operating systems to do the rain dance to acquire it. For most simulation work, it is much
  better to use [[generate-seed]], which returns fairly good pseudo-random numbers that are further
  used as seeds of pseudo-random number generators."
    ^long []
    (.getLong (ByteBuffer/wrap (.generateSeed srng Long/BYTES)) 0)))

(defn generate-seed
  "Generates a pseudo-random seed for jump-starting pseudo-random generators. Random enough for most
  simulation algorithms that we use in Uncomplicate, and very cheap to acquire."
  ^long []
  (- (long (rand Long/MAX_VALUE)) (quot Long/MAX_VALUE 2)))

;; ===================== Work Groups utilities ===================================

(defn count-groups
  "Calculates the number of groups of certain size in the total number of items."
  ^long [^long per-group ^long items]
  (if (< per-group items)
    (quot (+ items (dec per-group)) per-group)
    1))
