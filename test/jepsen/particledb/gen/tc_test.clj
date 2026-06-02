(ns jepsen.particledb.gen.tc-test
  (:require [clojure [pprint :refer [pprint]]]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check [generators :as g]
                                [properties :as prop]
                                [rose-tree :as rose]]
            [dom-top.core :refer [loopr]]
            [jepsen.particledb.gen.tc :refer :all]))
; Vector shrinking is not particularly good at shrinking a history of
; operations performed on a stateful system. We build a custom generator
; which shrinks differently.
;
; To demonstrate this, let's pretend we had a state machine which takes tuples
; of a keyword and an integer.

(def gen-op
  "A generator for a single state machine operation."
  ; Intentionally using bind here to make shrinking a little tougher
  (g/let [f (g/elements [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v])
          x (g/large-integer* {:min 0, :max 65536})]
    [f x]))

; Now let us imagine the state machine breaks iff it receives this specific
; series of tuples: [:a 3] [:b 2] [:c 1]. Other inputs do not affect
; correctness.

(defn breaks?
  "Is the given history of operations going to break the state machine?"
  [h]
  (loopr [state :a3]
         [[k v] h]
         (condp = [state k v]
           [:a3 :a 3] (recur :b2)
           [:b2 :b 2] (recur :c1)
           [:c1 :c 1] true
           (recur state))
         false))

; For n = 3, we want to shrink like so:
;
; [[:a 3] [:x 2] [:b 7] [:y 6] [:b 21] [:b 28] [:b 4] [:c 2]]
; [[:b 7] [:b 7] [:b 7]]
;
; We have filtered out the non-b, non-even, and zero inputs, and shrunk the
; even numbers to just 2. How does test.check handle this?

(deftest history-test
  (let [res (tc/quick-check
              10000
              (prop/for-all [h (g/let [n (g/large-integer* {:min 1, :max 65536})]
                                 (g/vector gen-op 0 n))]
                            (not (breaks? h)))
              :seed 1234)]
    ; Argh, it shrinks well. I KNOW this is broken in our generators... but
    ; why?
    ;(pprint (:shrunk res))
    (is (false? (:pass? res)))
    ;(is (= nil (first (:smallest (:shrunk res)))))
    ))

(deftest deletev-test
  (is (= [1 2] (deletev [0 1 2] 0)))
  (is (= [0 2] (deletev [0 1 2] 1)))
  (is (= [0 1] (deletev [0 1 2] 2))))

(deftest part-excisions-test
  (let [v [0 1 2 3 4 5 6 7 8]]
    (is (= [[0 1 2 3 4]
            [5 6 7 8]]
            (part-excisions v 2)))
    (is (= [[0 1 2 3 4 5]
            [0 1 2 6 7 8]
            [3 4 5 6 7 8]]
           (part-excisions v 3)))
    ; 0 1 | 2 3 4 | 5 6 | 7 8
    (is (= [[0 1 2 3 4 5 6]
            [0 1 2 3 4 7 8]
            [0 1 5 6 7 8]
            [2 3 4 5 6 7 8]]
           (part-excisions v 4)))
    (is (= nil
           (part-excisions v 5)
           (part-excisions v 6)
           (part-excisions v 9)
           (part-excisions v 145)))))

(deftest single-excisions-test
  (let [v [0 1 2 3 4 5 6 7 8]]
    (is (= [[0 1 2 3 4 5 6 7]
            [0 1 2 3 4 5 6 8]
            [0 1 2 3 4 5 7 8]
            [0 1 2 3 4 6 7 8]
            [0 1 2 3 5 6 7 8]
            [0 1 2 4 5 6 7 8]
            [0 1 3 4 5 6 7 8]
            [0 2 3 4 5 6 7 8]
            [1 2 3 4 5 6 7 8]]
           (single-excisions v)))))

(deftest excisions-test
  (let [v [0 1 2 3 4 5 6 7 8]]
    (is (= [; Split in half
            [0 1 2 3 4]
            [5 6 7 8]
            ; Split in quarters
            [0 1 2 3 4 5 6]
            [0 1 2 3 4 7 8]
            [0 1 5 6 7 8]
            [2 3 4 5 6 7 8]
            ; Individual excisions
            [0 1 2 3 4 5 6 7]
            [0 1 2 3 4 5 6 8]
            [0 1 2 3 4 5 7 8]
            [0 1 2 3 4 6 7 8]
            [0 1 2 3 5 6 7 8]
            [0 1 2 4 5 6 7 8]
            [0 1 3 4 5 6 7 8]
            [0 2 3 4 5 6 7 8]
            [1 2 3 4 5 6 7 8]]
           (excisions v)))))
