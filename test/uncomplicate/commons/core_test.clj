;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.commons.core-test
  (:require [clojure.set :refer [superset?]]
            [uncomplicate.commons.core :refer :all]
            [midje.sweet :refer [facts]])
  (:import [clojure.lang IFn IFn$D IFn$DD IFn$DDD IFn$DDDD IFn$DDDDD
            IFn$L IFn$LL IFn$LLL IFn$LLLL IFn$LLLLL]))

(facts "Primitivized functions should have primitive IFn$XXX types"
       (let [d+ (double-fn +)]
         (superset? (ancestors (type d+))
                    #{IFn IFn$D IFn$DD IFn$DDD IFn$DDDD IFn$DDDDD})
         (d+) => (double (+))
         (d+ 1) => (+ 1.0)
         (d+ 1 2) => (+ 1.0 2)
         (d+ 1 2 -5) => (+ 1.0 2 -5)
         (d+ 1 2 -5 100) => (+ 1.0 2 -5 100))

       (let [l+ (long-fn +)]
         (superset? (ancestors (type l+))
                    #{IFn IFn$L IFn$LL IFn$LLL IFn$LLLL IFn$LLLLL})
         (l+) => (+)
         (l+ 1) => (+ 1)
         (l+ 1 2) => (+ 1 2)
         (l+ 1 2 -5) => (+ 1 2 -5)
         (l+ 1 2 -5 100) => (+ 1 2 -5 100)))

(facts "Test Cleaner."
       (let [direct-buffer (java.nio.ByteBuffer/allocateDirect 8)]
         (release direct-buffer) => true))
