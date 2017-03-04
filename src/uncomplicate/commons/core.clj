;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.core
  (:import [java.nio ByteBuffer DirectByteBuffer]))

;; ===================== Releaseable =================================

(defprotocol Releaseable
  "Objects that hold resources that can be released after use. For OpenCL
  objects, releasing  means decrementing the reference count of the object.

  The errors should be signalled by throwing an exception rather than
  by the return value.
  "
  (release [this]
    "Releases the resource held by this."))

(extend-type java.lang.Object
  Releaseable
  (release [_]
    true))

(extend-type clojure.lang.Sequential
  Releaseable
  (release [this]
    (reduce #(release %2) true this)
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
    :else (throw (IllegalArgumentException.
                  "with-release only allows Symbols in bindings"))))

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
    :else (throw (IllegalArgumentException.
                  "try-release only allows Symbols in bindings"))))

(defprotocol Mappable
  (mmap [this] [this flags])
  (unmap [this mapped]))

;; =================== Array wrappers ==================================

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

;; ==================== Buffer utils ======================================

(defn clean-buffer
  "Cleans the direct byte buffer using JVM's cleaner, and releases the memory
  that resides outside JVM, wihich might otherwise linger very long until garbage
  collected. See the Java documentation for DirectByteBuffer for more info."
  [^ByteBuffer buffer]
  (when (.isDirect buffer)
    (.clean (.cleaner ^DirectByteBuffer buffer)))
  true)
