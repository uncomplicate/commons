;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.core
  (:import [java.nio ByteBuffer FloatBuffer DoubleBuffer LongBuffer IntBuffer DirectByteBuffer
            DirectFloatBufferU DirectDoubleBufferU DirectLongBufferU DirectIntBufferU]))

;; =================== Viewable ========================================

(defprotocol Viewable
  "Attach a default dense structure to the raw data of `x`. `x` can be anything that implements
  Viewable, such as DirectByteBuffer.

  Changes to the resulting object affect the source `x`, even the parts of data that might not
  be accessible by `x`. Use with caution!

  view always creates a new instance that reuses the master's data,
  but releasing a view never releases the master data.

      (view (buffer (vctr float-factory 1 2 3)))
  "
  (view [this]))

;; ===================== Releaseable =================================

(defprotocol Releaseable
  "Objects that hold resources that can be released after use. For OpenCL
  objects, releasing  means decrementing the reference count of the object.

  The errors should be signalled by throwing an exception rather than
  by the return value.
  "
  (release [this]
    "Releases the resource held by this."))

(defprotocol Info
  (info [this] [this info-type]))

(extend-type java.lang.Object
  Releaseable
  (release [_]
    true)
  Viewable
  (view [this]
    this)
  Info
  (info
    ([this]
     (str this))
    ([this _]
     (str this))))

(extend-type nil
  Releaseable
  (release [_]
    true)
  Viewable
  (view [_]
    nil)
  Info
  (info
    ([this]
     :nil)
    ([this _]
     :nil)))

(extend-type clojure.lang.IAtom
  Releaseable
  (release [this]
    (let [res (release @this)]
      (reset! this nil)
      res)))

(extend-type clojure.lang.Sequential
  Releaseable
  (release [this]
    (doseq [r this]
      (release r))
    true))

(extend-type ByteBuffer
  Releaseable
  (release [this]
    (.invokeCleaner (jdk.internal.misc.Unsafe/getUnsafe) this)
    true))

(extend-type FloatBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectFloatBufferU this)]
        (.clean cleaner)))
    true))

(extend-type DoubleBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectDoubleBufferU this)]
        (.clean cleaner)))
    true))

(extend-type IntBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectIntBufferU this)]
        (.clean cleaner)))
    true))

(extend-type LongBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectLongBufferU this)]
        (.clean cleaner)))
    true))

(defn releaseable?
  "Checks whether this is releaseable (in terms of Releaseable protocol)."
  [this]
  (satisfies? Releaseable this))

(defmacro with-release
  "Binds Releasable elements to symbols (like let do), evaluates
  body, and at the end releases the resources held by the bindings. The bindings
  can also be deeply sequential (see examples) - they will be released properly.

  Example:

      (with-release [devs (devices (first (platforms)))
                     dev (first devs)
                     ctx (context devs)
                     queue (command-queue ctx dev)]
        (info dev)
        (info queue))
  "
  [bindings & body]
  (assert (vector? bindings) "a vector for its binding")
  (assert (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-release ~(subvec bindings 2) ~@body)
                                (finally
                                  (release ~(bindings 0)))))
    :else (throw (IllegalArgumentException. "with-release only allows Symbols in bindings"))))

(defmacro let-release
  "Binds Releasable elements to symbols (like let do), evaluates
  body, and, if any exception occures, releases the resources held by the bindings.
  The bindings can also be deeply sequential (see examples)
  - they will be released properly.

  Example:

      (let-release [devs (devices (first (platforms)))
                    dev (first devs)
                    ctx (context devs)
                    queue (command-queue ctx dev)]
        (info dev)
        (info queue))
  "
  [bindings & body]
  (assert (vector? bindings) "a vector for its binding")
  (assert (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (let-release ~(subvec bindings 2) ~@body)
                                (catch Exception e#
                                  (do
                                    (release ~(bindings 0))
                                    (throw e#)))))
    :else (throw (IllegalArgumentException. "try-release only allows Symbols in bindings"))))

(defprotocol Mappable
  (mmap [this] [this flags])
  (unmap [this mapped]))

;; =================== Array wrappers ==================================

(defn wrap-byte ^bytes [^long x]
  (doto (byte-array 1) (aset 0 (byte x))))

(defn wrap-short ^shorts [^long x]
  (doto (short-array 1) (aset 0 (short x))))

(defn wrap-int ^ints [^long x]
  (doto (int-array 1) (aset 0 x)))

(defn wrap-long ^longs [^long x]
  (doto (long-array 1) (aset 0 x)))

(defn wrap-float ^floats [^double x]
  (doto (float-array 1) (aset 0 x)))

(defn wrap-double ^doubles [^double x]
  (doto (double-array 1) (aset 0 x)))

;; ==================== Primitive function builders =====================

(defmacro double-fn [f]
  `(fn
     (^double []
      (~f))
     (^double [^double x#]
      (~f x#))
     (^double [^double x# ^double y#]
      (~f x# y#))
     (^double [^double x# ^double y# ^double z#]
      (~f x# y# z#))
     (^double [^double x# ^double y# ^double z# ^double v#]
      (~f x# y# z# v#))))

(defmacro long-fn [f]
  `(fn
     (^long []
      (~f))
     (^long [^long x#]
      (~f x#))
     (^long [^long x# ^long y#]
      (~f x# y#))
     (^long [^long x# ^long y# ^long z#]
      (~f x# y# z#))
     (^long [^long x# ^long y# ^long z# ^long v#]
      (~f x# y# z# v#))))

;; =================== Wrappers ==========================

(defprotocol Wrapper
  (extract [this]))

(defprotocol Wrappable
  (wrap [this]))

(extend-type nil
  Wrapper
  (extract [_]
    nil)
  Wrappable
  (wrap [this]
    nil))
