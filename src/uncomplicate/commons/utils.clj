;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Dragan Djuric"}
    uncomplicate.commons.utils)

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
                 (if (= 0 (bit-and mask (long v)))
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
               (if (= 0 (bit-and mask (long v)))
                   nil
                   k))
             table)))

;; ====================== Error checking ==========================================

(defmacro with-check
  "Evaluates `form` if `err-code` is not zero (e.g. `CL_SUCCESS`), otherwise throws
  an appropriate `ExceptionInfo` with decoded informative details.
  It helps with native interop methods that return error codes directly, while
  returning computation results through side-effects in arguments.

  Example:

      (with-check (some-call-that-returns-error-code) result)
  "
  [error-fn err-code form]
  `(if (= 0 ~err-code)
     ~form
     (throw (~error-fn ~err-code ~(pr-str form)))))
