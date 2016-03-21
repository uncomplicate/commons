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
