(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.core)

(defprotocol Releaseable
  "Objects that hold resources that can be released after use. For OpenCL
  objects, releasing  means decrementing the reference count of the object.
  "
  (release [this]
    "Releases the resource held by this."))

(defn release-seq
  "if this is a releaseable object, releases it; if it is a (possibly nested)
  sequence of releaseable objects, calls itself on each element.
  "
  [this]
  (if (sequential? this)
    (map release-seq this)
    (release this)))

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
                                  (release-seq ~(bindings 0)))))
    :else (throw (IllegalArgumentException.
                  "with-release only allows Symbols in bindings"))))
