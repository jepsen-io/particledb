(ns jepsen.particledb.gen.tc
  "A test.check generator for a series of SQL statements."
  (:require [clojure [math :refer [round]]
                     [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [clojure.test.check :as tc]
            [clojure.test.check [generators :as g]
                                [properties :as prop]
                                [rose-tree :as rose]]
            [clojure.tools.logging :refer [info warn]]
            [com.gfredericks.test.chuck.generators :as g']
            [dom-top.core :refer [loopr]]
            [jepsen [util :as util]]))

;; Repl ideas
#_(do
    (require '[clojure.test.check.generators :as g])
    (require '[jepsen.particledb.gen.tc :as tc] :reload)
    (doseq [case (g/sample (tc/gen-case {:max-statement-count 100 :max-table-count 2 :max-column-count 2}) 1)]
      (println "\n\n# Case")
      (println "Value pool:")
      (pprint (:vpool (:schema case)))
      (println)
      (mapv println (:sql case)))
    )

;; Table mangling

(def table-start
  "A special character we use to identify the start of a table name."
  "\uF8FE")

(def table-end
  "A special character we use to identify the end of a table name."
  "\uF8FF")

(def table-pattern
  "A pattern to match a table."
  (re-pattern (str table-start "(.+?)" table-end)))

(defn mangle-table-str
  "Rewrites an sql strings's tables to include an (optional) id suffix."
  [id s]
  (str/replace s table-pattern (if id
                                 (str "$1_" id)
                                 "$1")))

;; General generators

(defn probability
  "A generator which returns true with probability p, otherwise false. Shrinks
  towards false. Only works up to one part in 1000."
  [p]
  (let [scale 1000
        t     (max 1 (Math/round (float (* scale p))))]
    (g/fmap (fn [x]
              (< x t))
            (g/choose 0 (dec scale)))))

(defn rarely
  "A generator which rarely generates from `rare`, but sometimes from `common`."
  [rare common]
  (g/frequency [[100 common]
                [1 rare]]))

(defmacro flet
  "A reduced version of `generators/let` which uses `fmap`, rather than `bind`,
  and does not support serial assignment; all bindings must be independent of
  one another. My hope is this shrinks better."
  [bindings & body]
  (assert (even? (count bindings)))
  (let [pairs  (partition 2 bindings)
        lefts  (mapv first pairs)
        rights (mapv second pairs)]
    `(g/fmap (fn ~'flet [~lefts]
               ~@body)
             (g/tuple ~@rights))))

(defn bind
  "It is SO frustrating that tc/gen does (fmap f gen) but (bind gen f)."
  [f gen]
  (g/bind gen f))

;; Basic types/schemas

(def column-names
  "All column names, as a vector."
  (mapv str "abcdefghijklmnopqrs"))

(def table-names
  "All table names, as a vector."
  (mapv (fn [name]
          (str table-start name table-end))
        ["t" "u" "v"]))

(def all-types-vec
  "All possible types, in order for shrinking."
  [:boolean
   :int
   :smallint
   :bigint
   :real
   :double
   :text
   :character3
   :character-varying3
   ; There's a point column type, but apparently no point(x, y) constructor?
   ;:point
   ; Apparently the parser doesn't know about the B'010' syntax???
   ;:bit3
   ;:bit-varying
   ])

(def all-types
  "All possible types, as a set."
  (set all-types-vec))

(def num-types
  "All numeric types, as a set."
  #{:smallint :int :bigint :real :double})

(def text-types
  "All textual types, as a set."
  #{:character3 :character-varying3 :text})

(def gen-column-type
  "A generator for a column type."
  (g/elements all-types-vec))

(defn general-type
  "Takes a type and returns either a set of compatible types, or the type
  itself. We use this to generate (e.g.) assignments from float to int."
  [type]
  (case type
    :smallint           num-types
    :int                num-types
    :bigint             num-types
    :real               num-types
    :double             num-types
    :character3         text-types
    :character-varying3 text-types
    :text               text-types
    type))

;; Combinators


(defn nullable
  "Takes a generator and returns one that might be NULL, roughly one time in
  five."
  [gen]
  (g/frequency [[1 (g/return "NULL")]
                [5 gen]]))

;; Literals

(def gen-boolean-lit
  "Generates a boolean literal."
  (nullable
    (g/elements ["TRUE" "FALSE"])))

(defn gen-string-lit
  "Generates a string literal. We're just doing simple strings for now. With no
  args, generates variable width strings. With an integer size, generates
  strings that *mostly* fit there."
  ([]
   (nullable
     (g/fmap (fn [s] (str "'" s "'"))
             (g'/string-from-regex #"[A-Za-z0-9\- _]+"))))
  ([n]
   (nullable
     (flet [s (g'/string-from-regex
                (re-pattern (str "[A-Za-z0-9\\- _]{0," (inc n) "}")))]
           (str "'" s "'")))))

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

(defn gen-bit-lit
  "Generates a bit string literal for a type like :bit3."
  [type]
  (case type
    :bit3        (g'/string-from-regex #"B'[01]{3}'")
    :bit-varying (g'/string-from-regex #"B'[01]+'")))

(def gen-point-lit
  "Generates a point literal like (1.23, 4.6)"
  (flet [x (g/double* {:infinite? false, :NaN? false})
         y (g/double* {:infinite? false, :NaN? false})]
        (str "point(" x ", " y ")")))

(defn gen-lit*
  "Takes a type and produces a generator of a literal value for that type."
  [type]
  (if (set? type)
    (g/one-of (mapv gen-lit* type))
    (case type
      :bigint             gen-bigint-lit
      :bit3               (gen-bit-lit type)
      :bit-varying        (gen-bit-lit type)
      :boolean            gen-boolean-lit
      :character3         (gen-string-lit 3)
      :character-varying3 (gen-string-lit 3)
      :double             gen-double-lit
      :int                gen-int-lit
      :point              gen-point-lit
      :real               gen-real-lit
      :smallint           gen-smallint-lit
      :text               (gen-string-lit))))

(defn gen-lit-dense
  "Takes a schema and a type, and returns a generator of literal values of that
  type which are 'dense', in the sense that they frequently return the same
  values. We use the schema's vpool for this."
  ([schema type]
   (assert (:vpool schema))
   (if (set? type)
     (g/one-of (mapv (partial gen-lit-dense schema) type))
     (let [{:keys [integers floats strings]} (:vpool schema)]
       (case type
         :bigint             (g/elements integers)
         :character3         (g/elements strings)
         :character-varying3 (g/elements strings)
         :double             (g/elements floats)
         :int                (g/elements integers)
         :real               (g/elements floats)
         :smallint           (g/elements integers)
         :text               (g/elements strings)
         (gen-lit* type))))))

(defn gen-lit
  "Takes a schema and a type and returns a generator of literal values for that
  type. Sometimes picks dense options from the schema, other times uses the raw
  generators for more diverse inputs."
  [schema type]
  (g/one-of [(gen-lit-dense schema type)
             (gen-lit* type)]))

(defn gen-vpool
  "A pool of literal values. We use this pool to generate the same values over
  and over again, which is helpful for (e.g.) producing joins between tables,
  or selecting via equality predicates.

    {:integer  [\"1\", \"4\"]
     :float    [\"1.23\"]
     :text     [\"'foo'\" ...]}"
  []
  ; The idea here is that 1-4 distinct values ought to be enough--remember we
  ; also mix tehse with other random values.
  (let [opts {:min-elements 1 :max-elements 4}]
    (g/hash-map
      ; We pick small generators here so that they're likely to work across
      ; domains
      :integers (g/vector-distinct gen-smallint-lit opts)
      :floats   (g/vector-distinct gen-real-lit opts)
      :strings  (g/vector-distinct (gen-string-lit 3) opts))))

(defn gen-expr-leaf
  "A generator for a simple expression of (usually) the given type which is
  (usually) valid in the context of the given table. For instance, 2, 'foo', or
  a, assuming a is a column of the right type."
  [schema table type]
  (let [lit (gen-lit schema type)
        cols (->> (:cols table)
                  (filter (if (keyword? type)
                            ; Keyword match
                            (comp #{type} :type)
                            ; Set match
                            (comp type :type)))
                  (mapv :name))
        ; If there's no columns, we don't have a generator here
        col  (when (seq cols)
               (g/elements cols))
        ; Sometimes, make up a random name just for grins. a-f means we mostly
        ; generate column names, but sometimes not, and sometimes of the wrong
        ; type.
        col!? (g/elements ["a" "b" "c" "d" "e" "f"])]
    (g/one-of
      (if col
        [lit col col!?]
        [lit col!?]))))

(defn gen-type
  "A generator for a type in the given table."
  [table]
  (->> (:cols table)
       (map :type)
       distinct
       vec
       g/elements
       (g/fmap general-type)))

;; Schemas

(defn gen-column-schema
  "Generates a column schema. A column schema is a map of the form:

  {:name         \"c1\"
   :type         :int
   :not-null?    false
   :unique?      false
   :primary-key? true}"
  [name]
  (flet [type          gen-column-type
         not-null?     g/boolean
         unique?       g/boolean
         primary-key?  g/boolean]
        (cond-> {:name name
                 :type type}
          not-null?    (assoc :not-null? true)
          unique?      (assoc :unique? true)
          primary-key? (assoc :primary-key? true))))

(defn column-schema->sql
  "Turns a column schema into the SQL to create it."
  [{:keys [type primary-key? unique? not-null?] :as col}]
  (str (:name col) " "
       (case type
         :bit3               "bit(3)"
         :character3         "character(3)"
         :character-varying3 "character varying(3)"
         :double             "double precision"
         (name               type))
       (when (text-types type)
         " COLLATE unicode")
       (when not-null? " NOT NULL")
       (when unique? " UNIQUE")
       (when primary-key? " PRIMARY KEY")
         ))

(defn remove-duplicate-primary-keys
  "Takes a vector of columns and limits them to just one primary key."
  [cols]
  (loopr [pk?   false
          cols' []]
         [col cols]
         (let [pk?' (or pk? (:primary-key? col))]
           (recur pk?'
                  (conj cols'
                        (if (and pk? pk?')
                          (dissoc col :primary-key?)
                          col))))
         cols'))

(defn gen-table-schema
  "Generates a table schema. A table schema is a map of the form:

  {:name \"c1\"
  :cols [column-schema ...]}

  Options are:

  :max-column-count The maximum number of columns"
  [opts name]
  ; We're doing a weird dance here to miminize the recurrent use of bind--this
  ; should hopefully shrink better.
  (let [mcc (:max-column-count opts (count column-names))]
    (->> (g/tuple (g/choose 1 mcc)
                  (g/shuffle (vec (take mcc column-names)))
                  (probability 1/100))
         ; Generate columns
         (bind (fn [[col-count col-names duplicate-pks?]]
                 (g/tuple
                   (->> col-names
                        (take col-count)
                        (mapv gen-column-schema)
                        (apply g/tuple))
                   (g/return duplicate-pks?))))
         ; Filter columns and produce a schema
         (g/fmap (fn [[cols duplicate-pks?]]
                   (let [cols (if duplicate-pks?
                                cols
                                (remove-duplicate-primary-keys cols))]
                     {:name name
                      :cols cols}))))))

(defn table-schema->sql
  "Turns a table schema into the SQL to create it."
  [{:keys [name cols]}]
  (str "CREATE TABLE " name " ("
       (str/join ", " (map column-schema->sql cols)) ")"))

(defn gen-schema
  "Generates a schema. A schema is a map of:

      :tables     A vector of table schemas.
      :vpool      A map of types to a few values, like {:integer [\"3\"
                  \"4\"]}. We use this to repeat the same values often in
                  (e.g.) equality predicates.

  Options:

      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts]
  (->> (g/tuple
         (g/choose 1 (:max-table-count opts (count table-names)))
         (g/shuffle table-names)
         (gen-vpool))
       (bind (fn [[table-count table-names vpool]]
               (g/hash-map
                 :vpool  (g/return vpool)
                 :tables (->> table-names
                              (take table-count)
                              (mapv (partial gen-table-schema opts))
                              (apply g/tuple)))))))

(defn schema->create-sql
  "Turns a schema into the SQL statements to create it."
  [schema]
  (mapv table-schema->sql (:tables schema)))

(defn schema->teardown-sql
  "Turns a schema into the teardown SQL statements to destroy it."
  [schema]
  (mapv (fn [{:keys [name]}]
          (str "DROP TABLE IF EXISTS " name))
        (:tables schema)))


;; Expression generation

; When generating recursive expressions like ((a > 3) AND (c IS NULL)), it's
; extremely handy to know what type of expression a generator will produce.
; Test.check generators are records, so we take advantage of that to assoc a
; :type field onto them, and use that in constructing branch expressions.

(declare gen-expr
         gen-expr-mostly-same-type)

;; Numeric expressions

(defn gen-expr-num-binary
  "Takes a generator of numeric expressions, and returns a generator of binary
  numeric expressions, like (1 + b)"
  [num-gen]
  (assoc (->> (g/tuple (g/elements ["+" "-" "*" "/" "%" "^"])
                       num-gen
                       num-gen)
              (g/fmap (fn glue [[op a b]]
                        (str "(" a " " op " " b ")"))))
         :type :num))

(defn gen-expr-num
  "Generator of a numeric expression, like 5, c, or (a + 2)"
  [schema table]
  (assoc
    (g/recursive-gen
      gen-expr-num-binary
      (gen-expr-leaf schema table num-types))
    :type :num))

;; Boolean expressions

(defn gen-expr-boolean-null
  "A generator of expressions which check whether a generator's value is or is
  not null."
  [expr-gen]
  (assoc
    (->> (g/tuple expr-gen g/boolean)
         (g/fmap (fn [[expr not?]]
                   (str "(" expr " IS " (when not? "NOT ") "NULL)"))))
    :type :boolean))

(def gen-binary-comparator
  "A generator for binary comparators like <, =, !=, ..."
  (g/elements ["=" "!=" "<" ">" "<=" ">="]))

(defn gen-expr-boolean-compare
  "Takes a generator of expressions, and returns a generator for boolean
  expressions comparing two expressions."
  [expr-gen]
  (assoc
    (->> (g/tuple gen-binary-comparator expr-gen expr-gen)
         (g/fmap (fn [[op a b]]
                   (str "(" a " " op " " b ")"))))
    :type :boolean))

(defn gen-expr-boolean-logic
  "Takes a generator of expressions, and produces a generator of boolean
  logical expressions like (a AND TRUE) or (NOT c)."
  [expr-gen]
  (assoc
    (g/one-of
      [; NOT
       (g/fmap (fn [x] (str "(NOT " x ")")) expr-gen)
       ; AND/OR
       (->> (g/tuple (g/elements ["AND" "OR"]) expr-gen expr-gen)
            (g/fmap (fn [[op a b]] (str "(" a " " op " " b ")"))))])
    :type :boolean))

; This is sort of a gross hack. Generating boolean recursive expressions means
; generating arbitrary expressions, so gen-expr-boolean needs gen-expr.
; gen-expr needs to generate boolean expressions, so it needs gen-expr-boolean.
; That mutual recursion happens eagerly at generator construction, so you get
; an infinite loop. I've tried several approaches to make it either lazy or to
; break the mutuality, and all of them seem bad. Also, using the usual
; recursive here tends to blow up the stack as it generates *very* deep
; structures. We use an explicit depth limit instead.

(defn gen-expr-boolean
  "Takes a table, and builds a generator of possibly recursive boolean
  expressions like TRUE, or (a = (b - 5)), up to the given depth. 0 indicates
  leaf expressions only."
  ([schema table]
   (gen-expr-boolean schema table 1))
  ([schema table depth]
   (if (< depth 1)
     (gen-expr-leaf schema table :boolean)
     (g/one-of [; Our leaves can be simple boolean literals or columns
                (gen-expr-leaf schema table :boolean)
                ; Or IS NULL expressions over any expression
                (gen-expr-boolean-null
                  (gen-expr schema table all-types (dec depth)))
                ; Or a boolean comparison over two elements, hopefully of
                ; similar types.
                (gen-expr-boolean-compare
                  (gen-expr-mostly-same-type schema table (dec depth)))
                ; Or a boolean logic combination over ourselves
                (gen-expr-boolean-logic
                  (gen-expr-boolean schema table (dec depth)))]))))

(defn gen-expr
  "Takes a table and a type (including a set of types), and and a depth bound.
  Returns a generator of expressions of that type. Depth 0 means only
  terminals."
  ([schema table]
   (gen-expr schema table all-types))
  ([schema table type]
   ; We allow up to two levels of general/boolean recursion
   (gen-expr schema table type 1))
  ([schema table type depth]
   ; Weirdly case won't dispatch correctly on all-types...
   (condp = type
     ; Compound expression generators
     :boolean  (gen-expr-boolean schema table depth)
     :smallint (gen-expr-num schema table)
     :int      (gen-expr-num schema table)
     :bigint   (gen-expr-num schema table)
     :real     (gen-expr-num schema table)
     :double   (gen-expr-num schema table)
     num-types (gen-expr-num schema table)
     all-types (g/one-of [(gen-expr-leaf schema table all-types)
                          (gen-expr-boolean schema table depth)
                          (gen-expr-num schema table)])

     ; Anything else, we can only generate a leaf
     (gen-expr-leaf schema table type))))

(defn gen-expr-mostly-same-type
  "Given a table, picks a type in the table, and constructs expressions which
  are usually (but not always) of the same type, up to the given recursion
  depth."
  [schema table depth]
  (g/bind (gen-type table)
          (fn [type]
            (g/frequency [[10 (gen-expr schema table type depth)]
                          [1  (gen-expr schema table all-types depth)]]))))

;; Statements

(defn gen-insert
  "Generates an INSERT statement."
  [schema]
  (g/let [table (g/elements (:tables schema))
          cols  (g/shuffle (:cols table))
          vals  (->> (mapv (comp (partial gen-lit schema) :type) cols)
                     (apply g/tuple))]
    (str "INSERT INTO " (:name table)
         " (" (str/join ", " (map :name cols)) ")"
         " VALUES (" (str/join ", " vals) ")")))

(defn gen-where?
  "Generates a (possibly empty) WHERE clause."
  [schema table]
  (g/frequency
    [[1 (g/return nil)]
     [2 (->> (gen-expr-boolean schema table)
             (g/fmap (fn [w] (str " WHERE " w))))]]))

(defn gen-update-pair
  "Takes a table and returns a generator of [col val] pairs, where col is a
  column name and val is an expression."
  [schema table]
  (g/let [col (g/elements (:cols table))
          val (gen-expr schema table (:type col))]
    [(:name col) val]))

(defn gen-update
  "Generates an UPDATE statement."
  [schema]
  (g/let [table (g/elements (:tables schema))
          ; TODO: generate duplicate assignments.
          pairs (let [pairs (gen-update-pair schema table)
                      n     (count (:cols table))]
                  (rarely
                    ; With dups
                    (g/vector (gen-update-pair schema table) 1 (inc n))
                    ; No dups
                    (g/vector-distinct-by first
                                          (gen-update-pair schema table)
                                          {:min-elements 1
                                           :max-elements n})))
          where (gen-where? schema table)]
    (str "UPDATE " (:name table) " SET "
         (str/join ", "
                   (mapv (fn [[k v]]
                           (str k " = " v))
                         pairs))
         where)))

(defn gen-limit?
  "Maybe generates a LIMIT clause."
  []
  (g/frequency [[1 (g/return nil)]
                [2 (flet [n (rarely (g/elements [0 -1])
                                    (g/large-integer* {:min 1}))]
                         (str " LIMIT " n))]]))

(defn gen-offset?
  "Maybe generates an OFFSET clause."
  []
  (g/frequency [[1 (g/return nil)]
                ; These tables won't get very big, so we want to focus on small
                ; offsets.
                [2 (flet [n (g/large-integer* {:min -1, :max 16})]
                         (str " OFFSET " n))]]))

(def gen-order-dir
  "Generates an ordering direction like ASC NULLS LAST"
  (flet [dir   (g/elements ["" " ASC" " DESC"])
         nulls (g/elements ["" " NULLS FIRST" " NULLS LAST"])]
        (str dir nulls)))

(defn gen-order?
  "Maybe generates an ORDER BY clause"
  [table]
  (g/frequency
    [[1 (g/return nil)]
     [1 (flet [col (g/elements (:cols table))
               dir gen-order-dir]
              (str " ORDER BY " (:name col) dir))]]))

(defn gen-total-order
  "Generates a total ordering clause for the given table."
  [table]
  (flet [cols (g/shuffle (mapv :name (:cols table)))
         dirs (g/vector gen-order-dir (count (:cols table)))]
        (str " ORDER BY "
             (->> (map str cols dirs)
                  (str/join ", ")))))

(defn gen-limit-offset-order?
  "Maybe generates a clause for LIMIT, OFFSET, and ORDER BY"
  [table]
  ; We want to get the same rows from this query when run against both
  ; databases, but where the sort order is underspecified, they're legally
  ; allowed to return different rows. To avoid this, we either generate a total
  ; order, OR omit the limit and offset clauses (and in this case, we just
  ; check set equivalence, not ordering).
  (g/one-of
    [; An order, no limit/offset
     (gen-order? table)
     ; Total order plus limit-offset
     (flet [order  (gen-total-order table)
            limit  (gen-limit?)
            offset (gen-offset?)]
           (str order offset limit))]))

(defn gen-select-cols
  "Generates the columns to select in a SELECT statement."
  [table]
  (let [n (count (:cols table))]
    (g/one-of [(flet [cols (g/vector (g/elements (:cols table))
                                     1 n)
                      ases (g/vector (g/one-of [(g/return nil)
                                                (g/elements column-names)])
                                     1 n)]
                     (->> (map (fn [col as]
                                 (str (:name col)
                                      (when as
                                        (str " AS " as))))
                               cols
                               ases)
                          (str/join ", ")))
               ; Everything
               (g/return "*")
               ; Aggregate
               (g/return "COUNT(*)")])))

(defn qualify
  "Takes a table and a column, and returns the qualified column {:name \"t.i\"
  :type ...}"
  [table column]
  (assoc column :name (str (:name table) "." (:name column))))

(defn qualify-cols
  "Takes a table and returns a vector of its columns, qualified."
  [table]
  (mapv (partial qualify table) (:cols table)))

(defn merge-tables
  "Takes a vector of tables and produces a merged table with qualified columns
  from each."
  [tables]
  {:name ::merged
   :cols (->> tables
              (mapcat qualify-cols)
              vec)})

(defn select-join-cols
  "Takes two tables, t and t', and an integer i. Returns [col, col']: two
  qualified columns drawn from t and t' respectively, which are hopefully of
  compatible types. Uses i to pick an option if there are multiple compatible
  columns. Each column is of the form:

    {:name \"t.i\"
     :type ...}"
  [t t' i]
  (let [cols  (group-by (comp general-type :type) (:cols t))
        cols' (group-by (comp general-type :type) (:cols t'))
        ; Types in common
        types (vec (set/intersection (set (keys cols))
                                     (set (keys cols'))))
        ; Pick candidate columns
        [cols cols']
        (if (seq types)
          ; We have a type in common
          (let [type (nth types (mod i (count types)))]
            [(get cols  type)
             (get cols' type)])
          ; As a fallback, pick random cols and see what happens
          [(:cols t) (:cols t')])
        ; Get a little more randomness
        j   (hash i)
        k   (hash j)]
    ; Pick specific columns
    [(qualify t  (nth cols  (mod j (count cols))))
     (qualify t' (nth cols' (mod j (count cols'))))]))

(defn gen-select-join
  "A generator for a JOIN clause from the current table to the given table'."
  [schema table table']
  (flet [dir    g/boolean        ; Direction for the join predicate
         i      g/small-integer ; Which compatible column do we use?
         type (g/elements [""
                           " INNER"
                           " LEFT"
                           " RIGHT"
                           " FULL"
                           " CROSS"])]
        (let [; Randomize whether we join a = b or b = a
              [t t'] (if dir [table table'] [table' table])
              ; Find columns of compatible types
              [col col'] (select-join-cols t t' i)]
          (str type " JOIN " (:name table')
               " ON " (:name col) " = " (:name col')))))

(defn gen-select-joins
  "A generator for a series of JOIN clauses between several tables, assuming
  the first table is the one we start selecting from."
  [schema [table & join-tables :as tables]]
  (if (seq join-tables)
    (->> (g/tuple
           ; The tables we will join to
           (g/shuffle join-tables)
           ; The tables we'll join *from*, in corresponding order
           (g/vector (g/elements tables) (count join-tables)))
         ; Ensure that we don't try to join tables to themselves
         (g/such-that (fn [[ts ts']]
                        (every? identity (map not= ts ts'))))
         ; Produce a vector of join clauses
         (bind (fn [[join-tables from-tables]]
                 (apply g/tuple (mapv (partial gen-select-join schema)
                                      from-tables
                                      join-tables))))
         ; Glue together
         (g/fmap (partial str/join " ")))
    ; No other tables
    (g/return nil)))

(defn gen-select-tables
  "Takes a schema and returns a generator of tables to join together for a
  SELECT. At least one table; the base table we start with."
  [schema]
  ; TODO: maybe *not* distinct?
  (g/vector-distinct (g/elements (:tables schema))
                     {:min-elements 1
                      :max-elements (count (:tables schema))}))

(defn gen-select
  "Generates a SELECT statement."
  [schema]
  (->> (gen-select-tables schema)
       (bind (fn [[table0 & more-tables :as tables]]
               (let [table (merge-tables tables)]
                 (flet [cols   (gen-select-cols table)
                        joins  (gen-select-joins schema tables)
                        where  (gen-where? schema table)
                        limit-offset-order (gen-limit-offset-order? table)]
                       (str "SELECT " cols
                            " FROM " (:name table0)
                            joins
                            where
                            limit-offset-order)))))))

(defn gen-delete
  "Generates a DELETE statement"
  [schema]
  (->> (g/elements (:tables schema))
       (bind (fn [table]
               (flet [where (gen-where? schema table)]
                     (str "DELETE FROM " (:name table) where))))))

(defn gen-statement
  "Generates a statement for a schema."
  [schema]
  (g/frequency [[8  (gen-select schema)]
                [8  (gen-insert schema)]
                [4  (gen-update schema)]
                [1  (gen-delete schema)]
                [2  (g/elements ["BEGIN" "COMMIT" "ROLLBACK"])]]))

(defn balance-txns
  "Adds commits for all open transactions in a sequence of statements."
  [statements]
  (loopr [depth 0]
         [sql statements]
         (case sql
           "BEGIN"    (recur (inc depth))
           "COMMIT"   (recur (max 0 (dec depth)))
           "ROLLBACK" (recur (max 0 (dec depth)))
           (recur depth))
         (into statements
               (repeat depth "COMMIT"))))

(defn gen-case
  "Generates a test case: a map of:

      :sql          A vector of SQL statements
      :teardown-sql A vector of SQL statements to tear down what this made.

  Options are:

      :max-statement-count  The maximum number of statements
      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts]
  (g/let [schema      (gen-schema opts)
          statements  (g/vector (gen-statement schema) 1
                                (:max-statement-count opts))]
    {:schema  schema
     :sql (into (schema->create-sql schema)
                (balance-txns statements))
     :teardown-sql (schema->teardown-sql schema)}))

;; Custom shrinking

(defn deletev
  "Deletes a single element from a vector."
  [v i]
  (into (subvec v 0 i)
        (subvec v (inc i))))

(defn single-excisions
  "Takes a vector of xs, and returns a vector of versions of xs, each with a
  single element deleted. Returns nil when xs is empty."
  [xs]
  (let [c (count xs)]
    (when (< 0 c)
      ; Deleting from last to first
      (->> (range (dec c) -1 -1)
           (map (partial deletev xs))))))

(defn part-excisions
  "Takes a vector xs. Divides xs into n parts, and produces a lazy sequence of
  n versions of xs, each with a single part excised. Nil if there are too few
  xs."
  [xs n]
  (assert (< 1 n))
  (let [c (count xs)]
    ; Some of the parts will have only a single element; we return nil when
    ; there are too many parts to signal the shift to single-excisions.
    (when (<= (* n 2) c)
      (let [cuts (-> (range 0 c (/ c n))
                     (->> (mapv round))
                     (conj c))
            parts (->> cuts
                       (partition 2 1)
                       (mapv (fn part [[i i']]
                               (subvec xs i i'))))]
        (map (fn excise [i]
                (reduce into (deletev parts i)))
             (reverse (range n)))))))

(defn excisions
  "Takes a vector xs. Returns a lazy sequence of versions of xs with various
  regions deleted, down to single-element removals."
  ([xs]
   (excisions xs 2))
  ([xs part-size]
   ; We do an explicit lazy-seq to avoid chunking on exponentially growing part
   ; numbers
   (lazy-seq
     (if-let [xs' (part-excisions xs part-size)]
       (concat xs' (excisions xs (* part-size 2)))
       ; Ground out in individual deletions
       (single-excisions xs)))))

(defn history->rose
  "You've got a 100,000 operation history which triggers a bug, and want to
  find a smaller history that also reproduces it. This function takes a history
  vector and returns a rose tree of shrunken history vectors from it.

  The end of the history is meaningless: the bug happened already, and
  executing those operations is wasted work. The first part is *also* probably
  meaningless: all that state got overwritten by later operations. We want to
  prune these bits *fast*. We produce a sequence of versions of the history
  missing the first half, then back half, then first quarter, then second
  quarter, and so on, down to individual element excisions."
  [history]
  (rose/make-rose history
                  (map history->rose
                       (excisions history))))

(defn case->rose
  "This is our custom Rose tree for shrinking a case. We only shrink the sql,
  and leave the initial DDL intact."
  [case]
  (let [sql       (:sql case)
        [ddl dml] (mapv vec (split-with (partial re-find #"^CREATE TABLE")
                                        sql))]
    ; We construct a rose tree of the DML, then expand each node into a case
    ; with the DML preamble, and ensure transactions are closed.
    (rose/fmap (fn [dml]
                 (->> (into ddl dml)
                      (balance-txns)
                      (assoc case :sql)))
               (history->rose dml))))

(defn case->gen
  "Turns a generated test case into a custom shrinking generator."
  [case]
  (-> case case->rose g/gen-pure))

(defn case-size
  "Roughly how big is a case?"
  [case]
  (str (-> case :sql count)
       " ops"))

(defn qc-report
  "A reporter fn to log quickcheck output."
  [{:keys [type pass? args fail] :as opts}]
  (case type
    :trial
    (info "QC pass" (case-size (first args)))

    :failure
    (info "QC fail" (case-size (first fail)))

    :shrink-step
    (info "QC shrink" (if (:pass? (:shrinking opts)) "pass" "fail")
          (case-size (first (:args (:shrinking opts)))))
    nil))

(declare origin-worker!)

(defn qc!
  "Runs quick-check on a case from the given generator, drawing IDs from the
  `last-id` atom. Returns quickcheck output. Options:

    :pool           The atom tracking origin worker state
    :seed           The random seed
    :test-cases     The number of test cases to run
    :target-origin  The origin string we're hunting for."
  [{:keys [pool seed test-cases target-origin] :as opts} gen queue last-id]
  (let [checked
        (tc/quick-check
          test-cases
          (prop/for-all [case gen]
                        (let [result  (promise)
                              id      (swap! last-id inc)
                              case    (assoc case
                                             :id     id
                                             :final? false
                                             :result result)]
                          ; Send case to generator
                          (swap! queue conj case)
                          ; Wait for a client to execute it
                          (let [{:keys [status origin]} @result]
                            ; If we see an origin, spawn a worker to hunt for
                            ; it.
                            (when origin
                              (origin-worker! (assoc opts :target-origin origin)
                                              last-id queue))
                            ; Is it interesting, and does it match the target
                            ; origin?
                            (not (and (= :interesting status)
                                      (or (nil? target-origin)
                                          (= target-origin origin)))))))

          :seed seed
          :reporter-fn qc-report)]
    (info "Test.check pass completed:\n"
          (with-out-str (pprint checked)))
    checked))

(defn worker-pass-1!
  "The first pass of our hunt: we generate increasingly large schemas, looking
  for bugs. Returns a test.check map. Options:

      :max-statement-count  The maximum number of statements
      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts last-id queue]
  ; We are *very* bad at shrinking schemas (for the usual test.check
  ; bind reasons), so we build a loop that gradually expands the
  ; schemas over time.
  (info "Pass 1: standard test.check search")
  (loop [max-table-count 2
         max-column-count 1]
    (info "Trying up to"
          max-table-count "tables and"
          max-column-count "columns")
    (let [checked (qc! opts
                       (gen-case
                         (assoc (select-keys opts [:max-statement-count])
                                :max-table-count max-table-count
                                :max-column-count max-column-count))
                       queue
                       last-id)]
      (cond ; Found a bug!
            (false? (:pass? checked))
            checked

            ; No bugs, and our schemas are large; give up
            (and (< 4 max-table-count)
                 (< 5 max-column-count))
            checked

            ; Let's keep hunting. Start off by adding a column
            (= 1 max-column-count)
            (recur max-table-count (inc max-column-count))

            ; When we get to n columns and n tables, start over
            ; with more tables.
            (= max-table-count max-column-count)
            (recur (inc max-table-count) 1)))))

(defn worker-pass-2!
  "The second pass of our hunt. We take an existing test case, construct a
  generator from it, and throw it back into test.check."
  [opts last-id queue case]
  (info "Pass 2: custom shrinking case of" (count (:sql case)) "operations")
  (let [checked (qc! opts (case->gen case) queue last-id)]
    (if-let [smallest (first (:smallest (:shrunk checked)))]
      (info "Pass 2: shrunk to" (case-size smallest))
      (info "Pass 2: did not shrink"))
    checked))

(defn origin-worker!
  "May start a test.check worker thread to chase down a specific origin string.
  Draws unique IDs from last-id, an atom holding an integer. Sends cases to the
  given atom holding a PersistentQueue. Options are:

      :pool           The atom tracking origin worker state
      :target-origin  The origin we're trying to find
      :test-cases     The number of test cases to run
      :seed           The random seed
      :max-statement-count  The maximum number of statements
      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [{:keys [pool target-origin] :as opts} last-id queue]
  ; Check whether anyone else is looking for this
  (let [[pool0] (swap-vals! pool (fn [pool]
                                   (if (contains? pool target-origin)
                                     pool
                                     (assoc pool target-origin :running))))]
    (when-not (contains? pool0 target-origin)
      ; We're the first to look for this origin
      (future
        (util/with-thread-name (str "jepsen test.check " target-origin)
          (info "Hunting for origin" target-origin)
          (try
            (let [checked (worker-pass-1! opts last-id queue)]
              (when-let [smallest (first (:smallest (:shrunk checked)))]
                (worker-pass-2! opts last-id queue smallest)))
            (catch Throwable t
              (warn t "Jepsen test.check origin worker crashed!")
              (throw t))
            (finally
              (info "Done hunting for origin" target-origin)
              ; Mark ourselves as complete. If we're the last worker, tell the
              ; generator queue it's OK to stop.
              (let [pool0 (swap! pool assoc target-origin :done)]
                (when (every? #{:done} (vals pool0))
                  (swap! queue conj :stop))))))))))

(defn worker
  "Starts a test.check worker thread. Draws unique IDs from last-id, an atom
  holding an integer. Sends cases to the given atom holding a PersistentQueue.
  Options are:

      :test-cases The number of test cases to run
      :seed       The random seed
      :max-statement-count  The maximum number of statements
      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns

  This worker manages other workers; it will spawn a series of origin workers
  to chase down each unique origin it finds."
  [opts last-id queue]
  (let [; This is a map of origin strings to origin worker states: initially
        ; :running, and :done when complete.
        pool (atom {:supervisor :running})
        opts (assoc opts :pool pool)]
  (future
    (util/with-thread-name "jepsen test.check"
      (try
        (let [checked (worker-pass-1! opts last-id queue)]
          (when-let [smallest (first (:smallest (:shrunk checked)))]
            (worker-pass-2! opts last-id queue smallest)))
        (catch Throwable t
          (warn t "Jepsen test.check worker crashed!")
          (throw t))
        (finally
          ; Mark ourselves as complete. If we're the last worker, tell the
          ; generator queue it's OK to stop.
          (let [pool0 (swap! pool assoc :supervisor :done)]
            (when (every? #{:done} (vals pool0))
              (swap! queue conj :stop)))))))))
