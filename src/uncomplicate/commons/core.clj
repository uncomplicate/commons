;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.core
  "Core Uncomplicate functions useful across all projects.

  Projects that use native libraries and FFI typically need to dispose resources properly.
  To make this uniform, the typical resource managers need to implement the [[Releaseable]]
  protocol. Clients can then use the following functions to manage resources:
  [[release]] [[releaseable?]], [[with-release]], [[let-release]].

  It is often the case that you need an access to a mutable resource, but you want to
  protect the object's resource from destruction. For such cases (and other cases
  when this might be useful) there is a [[Viewable]] protocol. The [[view]] function
  should return a 'portal' to the object whose [[release]] implementation does nothing
  to the underlying resource.

  For cases where you'd like to have the ability to inspect an object for various
  internal detail, but don't want to clutter its interface with many additional
  functions not related to the domain itself, implement the [[Info]] protocol.
  The [[info]] method returns a hash-map of all available information, or, if provided
  with a key, only the specific property related to that key.

  Objects that manage data of certain size can implement protocols [[Entries]] and [[Bytes]].
  Use the following functions to query for the data properties: [[size]], [[sizeof]], and [[bytesize]].

  The following functions might be useful from time to time when dealing with primitive types.

  [[types]], [[double-fn]], [[long-fn]],
  [[wrap-byte]] [[wrap-short]] [[wrap-int]] [[wrap-long]][[wrap-float]][[wrap-double]],
  [[Mappable]], [[map]], [[unmap]].

  Please refer to the tests folder, and to the source of other Uncomplicate projects to
  see how the functions from this namespace can be useful in various ways.
  "
  (:import java.util.Collection
           [java.nio ByteBuffer FloatBuffer DoubleBuffer LongBuffer IntBuffer ShortBuffer
            CharBuffer Buffer DirectByteBuffer DirectFloatBufferU DirectDoubleBufferU
            DirectLongBufferU DirectIntBufferU DirectShortBufferU DirectCharBufferU]
           [clojure.lang Sequential AReference Atom Ref Delay]))

(def ^{:const true
       :doc "Available mappings from keywords to Java primitive types."}
  types
  {:double Double/TYPE
   :float Float/TYPE
   :int Integer/TYPE
   :long Long/TYPE
   :short Short/TYPE
   :byte Byte/TYPE
   :char Character/TYPE
   :bool Boolean/TYPE})

(def ^{:const true
       :doc "Available mappings from keywords to Java primitive size in bytes."}
  types-size
  {:double Double/BYTES
   :float Float/BYTES
   :int Integer/BYTES
   :long Long/BYTES
   :short Short/BYTES
   :byte Byte/BYTES
   :char Character/BYTES
   :bool 1
   Double Double/BYTES
   Float Float/BYTES
   Integer Integer/BYTES
   Long Long/BYTES
   Short Short/BYTES
   Byte Byte/BYTES
   Character Character/BYTES
   Boolean 1
   double Double/BYTES
   float Float/BYTES
   int Integer/BYTES
   long Long/BYTES
   short Short/BYTES
   byte Byte/BYTES
   char Character/BYTES
   boolean 1})

(defmacro double-fn
  "Wraps a function into a primitive type-annotated function (to satisfy the compiler in some cases)."
  [f]
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

(defmacro long-fn
  "Wraps a function into a primitive type-annotated function (to satisfy the compiler in some cases)."
  [f]
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

(defprotocol Viewable
  "Attach a default dense structure to the raw data of `x`. `x` can be anything that implements
  Viewable, such as `DirectByteBuffer`, `Pointer`, vector, matrix, tensor, Java object etc.
  The default implementation just returns the object itself.
  Changes to the resulting object can affect the source `x`, even the parts of data that might not
  be accessible by `x`. Use with caution!
  Typically creates a new instance that reuses the master's data, but releasing a view should
  never release the master data.
  Many code examples are available throughout Uncomplicate libraries.
  "
  (view [this] "Views this through new managing instance."))

(defprotocol Info
  "Object that can provide detailed information maps."
  (info [this] [this info-type] "Provide all available information, or, spicific information if the corresponding `info-type` key is provided."))

(defprotocol Mappable
  "Objects whose memory can be mapped or unmapped."
  (mmap [this] [this flags] "Map this object.")
  (unmap [this mapped] "Unmap this object."))

(defprotocol Entries
  "Object that holds data that has total size in number of entries and size of one entry in bytes."
  (sizeof* [entry] "Size of one data entry in bytes.")
  (size* [this] "Number of entries in data."))

(defprotocol Bytes
  "Object that has"
  (bytesize* [this] "The size of this object's data in bytes."))

(defn size
  "The size of `x`'s data in number of entries."
  ^long [x]
  (long (size* x)))

(defn sizeof
  "The size of one data entry of `x`'s data in bytes."
  ^long [x]
  (long (sizeof* x)))

(defn bytesize
  "The size of `x`'s data in bytes."
  ^long [x]
  (long (bytesize* x)))

(defprotocol Releaseable
  "Objects that hold resources that can be released after use. For OpenCL
  objects, releasing means decrementing the reference count of the object.
  Pointers call JavaCPP deallocators, Vectors and matrices call `free!`, etc.
  The errors should be signalled by throwing an exception rather than
  by the return value.
  "
  (release [this] "Releases all resources held by this object."))

(defn releaseable?
  "Checks whether this is releaseable (in terms of Releaseable protocol)."
  [this]
  (satisfies? Releaseable this))

(defmacro with-release
  "Binds Releasable elements to symbols (like `let` does), evaluates `body`,
  and eventually releases the resources held by the bindings. The bindings can also
  be deeply sequential (see examples) - they should be released properly.

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
  "Binds Releasable elements to symbols (like `let` does), evaluates `body`,  and, if any exception
  occures, releases the resources held by the bindings. The bindings can also be deeply sequential
  (see examples) - they should be released properly.

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

(extend-type Object
  Viewable
  (view [this]
    this)
  Info
  (info
    ([this]
     (str this))
    ([this _]
     (str this)))
  Releaseable
  (release [_]
    true))

(extend-type Number
  Releaseable
  (release [_]
    true)
  Viewable
  (view [this]
    this)
  Info
  (info
    ([this]
     this)
    ([this _]
     this)))

(extend-type nil
  Releaseable
  (release [_]
    true)
  Viewable
  (view [_]
    nil)
  Info
  (info
    ([_]
     nil)
    ([_ _]
     nil))
  Bytes
  (bytesize* [_]
    0)
  Entries
  (sizeof* [_]
    0)
  (size* [_]
    0))

(extend-type AReference
  Info
  (info
    ([this]
     (info (deref this)))
    ([this info-type]
     (info (deref this) info-type))))

(extend-type Atom
  Releaseable
  (release [this]
    (let [res (release (deref this))]
      (reset! this nil)
      res)))

(extend-type Ref
  Releaseable
  (release [this]
    (let [res (release (deref this))]
      (ref-set this nil)
      res)))

(extend-type Delay
  Releaseable
  (release [this]
    (release (deref this))))

(extend-type Sequential
  Info
  (info
    ([this]
     (map info this))
    ([this info-type]
     (map #(info % info-type) this)))
  Releaseable
  (release [this]
    (doseq [e this]
      (release e))
    true)
  Entries
  (size* [this]
    (count this)))

(extend-type Collection
  Info
  (info
    ([this]
     (map info this))
    ([this info-type]
     (map #(info % info-type) this)))
  Releaseable
  (release [coll]
    (doseq [e coll]
      (release e))
    true)
  Entries
  (size* [this]
    (count this)))

(extend-type Buffer
  Bytes
  (bytesize* [this]
    (* (long (sizeof* this)) (.capacity this)))
  Entries
  (sizeof* [_]
    Byte/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type ByteBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectByteBuffer this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (.capacity this))
  Entries
  (sizeof* [_]
    Byte/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type FloatBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectFloatBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Float/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Float/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type DoubleBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectDoubleBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Double/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Double/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type IntBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectIntBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Integer/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Integer/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type LongBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectLongBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Long/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Long/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type ShortBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectShortBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Short/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Short/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type CharBuffer
  Releaseable
  (release [this]
    (when (.isDirect this)
      (when-let [cleaner (.cleaner ^DirectCharBufferU this)]
        (.clean cleaner)))
    true)
  Bytes
  (bytesize* [this]
    (* Character/BYTES (.capacity this)))
  Entries
  (sizeof* [_]
    Character/BYTES)
  (size* [this]
    (.capacity this)))

(extend-type Float
  Bytes
  (bytesize* [_]
    Float/BYTES)
  Entries
  (sizeof* [_]
    Double/BYTES)
  (size* [_]
    1))

(extend-type Double
  Bytes
  (bytesize* [_]
    Double/BYTES)
  Entries
  (sizeof* [_]
    Double/BYTES)
  (size* [_]
    1))

(extend-type Integer
  Bytes
  (bytesize* [_]
    Integer/BYTES)
  Entries
  (sizeof* [_]
    Integer/BYTES)
  (size* [_]
    1))

(extend-type Long
  Bytes
  (bytesize* [_]
    Long/BYTES)
  Entries
  (sizeof* [_]
    Long/BYTES)
  (size* [_]
    1))

(extend-type Short
  Bytes
  (bytesize* [_]
    Short/BYTES)
  Entries
  (sizeof* [_]
    Short/BYTES)
  (size* [_]
    1))

(extend-type Character
  Bytes
  (bytesize* [_]
    Character/BYTES)
  Entries
  (sizeof* [_]
    Character/BYTES)
  (size* [_]
    1))

(extend-type Byte
  Bytes
  (bytesize* [_]
    Byte/BYTES)
  Entries
  (sizeof* [_]
    Byte/BYTES)
  (size* [_]
    1))

(extend-type (Class/forName "[F")
  Bytes
  (bytesize* [this]
    (* Float/BYTES (alength ^floats this)))
  Entries
  (sizeof* [_]
    Float/BYTES)
  (size* [this]
    (alength ^floats this)))

(extend-type (Class/forName "[D")
  Bytes
  (bytesize* [this]
    (* Double/BYTES (alength ^doubles this)))
  Entries
  (sizeof* [_]
    Double/BYTES)
  (size* [this]
    (alength ^floats this)))

(extend-type (Class/forName "[I")
  Bytes
  (bytesize* [this]
    (* Integer/BYTES (alength ^ints this)))
  Entries
  (sizeof* [_]
    Integer/BYTES)
  (size* [this]
    (alength ^ints this)))

(extend-type (Class/forName "[J")
  Bytes
  (bytesize* [this]
    (* Long/BYTES (alength ^longs this)))
  Entries
  (sizeof* [_]
    Long/BYTES)
  (size* [this]
    (alength ^longs this)))

(extend-type (Class/forName "[S")
  Bytes
  (bytesize* [this]
    (* Short/BYTES (alength ^shorts this)))
  Entries
  (sizeof* [_]
    Short/BYTES)
  (size* [this]
    (alength ^shorts this)))

(extend-type (Class/forName "[B")
  Bytes
  (bytesize* [this]
    (alength ^bytes this))
  Entries
  (sizeof* [_]
    Byte/BYTES)
  (size* [this]
    (alength ^bytes this)))

(extend-type (Class/forName "[C")
  Bytes
  (bytesize* [this]
    (* Character/BYTES (alength ^chars this)))
  Entries
  (sizeof* [_]
    Character/BYTES)
  (size* [this]
    (alength ^chars this)))

(extend-type (Class/forName "[Z")
  Bytes
  (bytesize* [this]
    (* 2 (alength ^booleans this)))
  Entries
  (sizeof* [_]
    2)
  (size* [this]
    (alength ^booleans this)))

(defn wrap-byte
  "Wraps a long number with a primitive byte array"
  ^bytes [^long x]
  (doto (byte-array 1) (aset 0 (byte x))))

(defn wrap-short
  "Wraps a long number with a primitive short array"
  ^shorts [^long x]
  (doto (short-array 1) (aset 0 (short x))))

(defn wrap-int
  "Wraps a long number with a primitive int array"
  ^ints [^long x]
  (doto (int-array 1) (aset 0 x)))

(defn wrap-long
  "Wraps a long number with a primitive long array"
  ^longs [^long x]
  (doto (long-array 1) (aset 0 x)))

(defn wrap-float
  "Wraps a double number with a primitive float array"
  ^floats [^double x]
  (doto (float-array 1) (aset 0 x)))

(defn wrap-double
  "Wraps a double number with a primitive double array"
  ^doubles [^double x]
  (doto (double-array 1) (aset 0 x)))
