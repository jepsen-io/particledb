(ns jepsen.particledb.gen.tc
  "A test.check generator for a series of SQL statements."
  (:require [clojure [pprint :refer [pprint]]
             [set :as set]
             [string :as str]]
            [clojure.test.check :as tc]
            [clojure.test.check [generators :as g]
                                [properties :as prop]]
            [clojure.tools.logging :refer [info warn]]
            [com.gfredericks.test.chuck.generators :as g']
            [dom-top.core :refer [loopr]]))

;; General combinators

(defn rarely
  "A generator which rarely generates from `rare`, but sometimes from `common`."
  [rare common]
  (g/bind (g/double* {:min 0, :max 1})
          (fn decide [x]
            (if (< 0.99 x)
              rare
              common))))

;; Schemas

(def column-names
  "All column names, as a vector."
  ["a" "b" "c" "d" "e"])

(def table-names
  "All table names, as a vector."
  ["t" "u" "v"])

(def all-types-vec
  "All possible types, in order for shrinking."
  [:boolean :smallint :bigint :real :double :text])

(def all-types
  "All possible types, as a set."
  (set all-types-vec))

(def num-types
  "All numeric types, as a set."
  #{:smallint :bigint :real :double})

(def gen-column-type
  "A generator for a column type."
  (g/elements all-types-vec))

(defn general-type
  "Takes a type and returns either a set of compatible types, or the type
  itself. We use this to generate (e.g.) assignments from float to int."
  [type]
  (case type
    :smallint num-types
    :int      num-types
    :bigint   num-types
    :real     num-types
    :double   num-types
    type))

(defn nullable
  "Takes a generator and returns one that might be NULL, roughly one time in five."
  [g]
  (g/one-of [g g g g (g/return "NULL")]))

(def gen-boolean-lit
  "Generates a boolean literal."
  (nullable
    (g/elements ["TRUE" "FALSE"])))

(def gen-string-lit
  "Generates a string literal. We're just doing simple strings for now."
  (nullable
    (g/fmap (fn [s] (str "'" s "'"))
            (g'/string-from-regex #"[A-Za-z0-9\- _]+"))))

(defn gen-long
  "A generator of Long values between lower and upper, inclusive."
  [lower upper]
  (g/large-integer* {:min lower :max upper}))

(def enormous-int-lit
  "An integer literal that does not fit in bigint."
  (g/one-of [(-> Long/MAX_VALUE bigint (+ 1) str g/return)
             (-> Long/MIN_VALUE bigint (- 1) str g/return)]))

(def gen-smallint-lit
  "A generator for (mostly) 2-byte int literals."
  (nullable
    (rarely enormous-int-lit
            (gen-long Short/MIN_VALUE Short/MAX_VALUE))))

(def gen-int-lit
  "A generator for (mostly) 4-byte int literals."
  (nullable
    (rarely enormous-int-lit
            (gen-long Integer/MIN_VALUE Integer/MAX_VALUE))))

(def gen-bigint-lit
  "A generator for (mostly) 8-byte integer literals."
  (nullable
    (rarely enormous-int-lit
            (gen-long Long/MIN_VALUE Long/MAX_VALUE))))

(defn float->str
  "Converts a float to an SQL string."
  [f]
  (cond
    (string? f)   f
    (NaN? f)      "'NaN'"
    (infinite? f) (str "'" (str f) "'")
    true          (str f)))

(def gen-real-lit
  "Geneates a `real` float literal."
  (g/fmap float->str
          (rarely (g/elements [Double/MIN_VALUE
                               Double/MAX_VALUE
                               "1.23E400"
                               "-4.56E500"
                               ##-Inf
                               ##Inf
                               ##NaN
                               "NULL"])
                  (g/double* {:min Float/MIN_VALUE
                              :max Float/MAX_VALUE}))))

(def gen-double-lit
  "Generates a `double precision` float literal."
  (g/fmap float->str
          (rarely (g/elements
                    ["1.23E400"
                     "-4.56E500"
                     "NULL"])
                  g/double)))

(defn gen-lit
  "Takes a type and produces a generator of a literal value for that type."
  [type]
  (if (set? type)
    (g/one-of (mapv gen-lit type))
    (case type
      :boolean  gen-boolean-lit
      :smallint gen-smallint-lit
      :int      gen-int-lit
      :bigint   gen-bigint-lit
      :real     gen-real-lit
      :double   gen-double-lit
      :text     gen-string-lit)))

(defn gen-expr-leaf
  "A generator for a simple expression of (usually) the given type which is (usually)
  valid in the context of the given table. For instance, 2, 'foo', or a,
  assuming a is a column of the right type."
  [table type]
  (g/one-of
    [; Literal
     (gen-lit type)
     ; Column
     (->> (:cols table)
          (filter (if (keyword? type)
                    ; Keyword match
                    (comp #{type} :type)
                    ; Set match
                    (comp type :type)))
          vec
          g/elements)
     ; Sometimes, make up a random name just for grins. a-f means we mostly
     ; generate column names, but sometimes not, and sometimes of the wrong
     ; type.
     (g/elements ["a" "b" "c" "d" "e" "f"])]))

(defn gen-type
  "A generator for a type in the given table."
  [table]
  (->> (:cols table)
       (map :type)
       distinct
       vec
       g/elements
       (g/fmap general-type)))

;; Expression generation

(declare gen-expr
         gen-expr-num)

;; Numeric expressions

(defn gen-expr-num-binary
  "Generator of a binary numeric expression, like (1 + b)"
  [table]
  (g/let [a (gen-expr-num table)
          b (gen-expr-num table)
          op (g/elements ["+" "-" "*" "/" "%" "^"])]
    (str "(" a " " op " " b ")")))

(defn gen-expr-num
  "Generator of a numeric expression, like 5, c, or (a + 2)"
  [table]

