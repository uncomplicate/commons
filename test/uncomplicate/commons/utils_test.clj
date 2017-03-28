(ns uncomplicate.commons.utils-test
  (:require [uncomplicate.commons.utils :refer :all]
            [midje.sweet :refer [facts throws =>]]))

(facts
 "mask tests"
 (let [table {:a 1 :b 2 :c 4}]

   (mask table [:a :c]) => 5
   (mask table :a [:c]) => 5
   (mask table :a :c[]) => 5

   (mask table [:k]) => (throws NullPointerException)
   (mask {} [:a]) => (throws NullPointerException)))

(facts
 "unmask tests"
 (let [table {:a 1 :b 2 :c 4}]

   (unmask table 1) => '(:a)
   (unmask table 2) => '(:b)
   (unmask table 4) => '(:c)

   (unmask table 0) => '()
   (unmask table 3) => '(:a :b)
   (unmask table 5) => '(:a :c)
   (unmask table 7) => '(:a :b :c)

   (unmask table 10) => '(:b)

   (unmask nil 0) => '()))

(facts
 "unmask1 tests"
 (let [table {:a 1 :b 2 :c 4}]

   (unmask1 table 1) => :a
   (unmask1 table 2) => :b
   (unmask1 table 4) => :c

   (unmask1 table 0) => nil
   (unmask1 table 3) => :a
   (unmask1 table 5) => :a
   (unmask1 table 7) => :a

   (unmask1 table 10) => :b

   (unmask1 nil 0) => nil))

(facts
 "cond-into tests"
 (cond-into {:a 1} true [:b 2] false [:c 3] (< 2 4) [:d 4]) => {:a 1 :b 2 :d 4}
 (cond-into {:a 1}) => {:a 1}
 (cond-into [1 2] true :a :b false :c :d) => [1 2 :a false :d])
