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
            [dom-top.core :refer [loopr]]
            [jepsen [util :as util]]))

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

(defn dense
  "Takes a generator and produces one which tends to repeat earlier values.
  This means that we're likely to (e.g.) generate queries for the specific
  numbers we inserted earlier."
  [gen]
  ; TODO: no idea how to do this
  )

;; Basic types/schemas

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
         :double "double precision"
         (name type))
       (when not-null? " NOT NULL")
       (when unique? " UNIQUE")
       (when primary-key? " PRIMARY KEY")))

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
  (->> (g/tuple (g/choose 1 (:max-column-count opts (count column-names)))
                (g/shuffle column-names)
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
                    :cols cols})))))

(defn table-schema->sql
  "Turns a table schema into the SQL to create it."
  [{:keys [name cols]}]
  (str "CREATE TABLE " name " ("
       (str/join ", " (map column-schema->sql cols)) ")"))

(defn gen-schema
  "Generates a schema. A schema is a vector of table schemas. Options:

      :id The unique integer ID for this schema. We use this to choose unique
          table names.
      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts]
  (->> (g/tuple (g/choose 1 (:max-table-count opts (count table-names)))
                (g/shuffle table-names))
       (bind (fn [[table-count table-names]]
               (->> table-names
                    (take table-count)
                    (mapv (fn uniquify [table-name]
                            (str table-name "_" (:id opts))))
                    (mapv (partial gen-table-schema opts))
                    (apply g/tuple))))))

(defn schema->create-sql
  "Turns a schema into the SQL statements to create it."
  [schema]
  (mapv table-schema->sql schema))

(defn schema->teardown-sql
  "Turns a schema into the teardown SQL statements to destroy it."
  [schema]
  (mapv (fn [{:keys [name]}]
          (str "DROP TABLE IF EXISTS " name))
        schema))

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
  "A generator for a simple expression of (usually) the given type which is
  (usually) valid in the context of the given table. For instance, 2, 'foo', or
  a, assuming a is a column of the right type."
  [table type]
  (let [lit (gen-lit type)
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
  [table]
  (assoc
    (g/recursive-gen
      gen-expr-num-binary
      (gen-expr-leaf table num-types))
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
  ([table]
   (gen-expr-boolean table 1))
  ([table depth]
   (if (< depth 1)
     (gen-expr-leaf table :boolean)
     (g/one-of [; Our leaves can be simple boolean literals or columns
                (gen-expr-leaf table :boolean)
                ; Or IS NULL expressions over any expression
                (gen-expr-boolean-null (gen-expr table all-types (dec depth)))
                ; Or a boolean comparison over two elements, hopefully of
                ; similar types.
                (gen-expr-boolean-compare (gen-expr-mostly-same-type table (dec depth)))
                ; Or a boolean logic combination
                (gen-expr-boolean-logic
                  (gen-expr-boolean table (dec depth)))]))))

(defn gen-expr
  "Takes a table and a type (including a set of types), and and a depth bound.
  Returns a generator of expressions of that type. Depth 0 means only
  terminals."
  ([table]
   (gen-expr table all-types))
  ([table type]
   ; We allow up to two levels of general/boolean recursion
   (gen-expr table type 1))
  ([table type depth]
   ; Weirdly case won't dispatch correctly on all-types...
   (condp = type
     ; Compound expression generators
     :boolean  (gen-expr-boolean table depth)
     :smallint (gen-expr-num table)
     :int      (gen-expr-num table)
     :bigint   (gen-expr-num table)
     :real     (gen-expr-num table)
     :double   (gen-expr-num table)
     num-types (gen-expr-num table)
     all-types (g/one-of [(gen-expr-leaf table all-types)
                          (gen-expr-boolean table depth)
                          (gen-expr-num table)])

     ; Anything else, we can only generate a leaf
     (gen-expr-leaf table type))))

(defn gen-expr-mostly-same-type
  "Given a table, picks a type in the table, and constructs expressions which
  are usually (but not always) of the same type, up to the given recursion
  depth."
  [table depth]
  (g/bind (gen-type table)
          (fn [type]
            (g/frequency [[10 (gen-expr table type depth)]
                          [1  (gen-expr table all-types depth)]]))))

;; Statements

(defn gen-insert
  "Generates an INSERT statement."
  [schema]
  (g/let [table (g/elements schema)
          cols  (g/shuffle (:cols table))
          vals  (->> (mapv (comp gen-lit :type) cols)
                     (apply g/tuple))]
    (str "INSERT INTO " (:name table)
         " (" (str/join ", " (map :name cols)) ")"
         " VALUES (" (str/join ", " vals) ")")))

(defn gen-where?
  "Generates a (possibly empty) WHERE clause."
  [table]
  (g/frequency
    [[1 (g/return nil)]
     [2 (->> (gen-expr-boolean table)
             (g/fmap (fn [w] (str " WHERE " w))))]]))

(defn gen-update-pair
  "Takes a table and returns a generator of [col val] pairs, where col is a
  column name and val is an expression."
  [table]
  (g/let [col (g/elements (:cols table))
          val (gen-expr table (:type col))]
    [(:name col) val]))

(defn gen-update
  "Generates an UPDATE statement."
  [schema]
  (g/let [table (g/elements schema)
          ; TODO: generate duplicate assignments.
          pairs (let [pairs (gen-update-pair table)
                      n     (count (:cols table))]
                  (rarely
                    ; With dups
                    (g/vector (gen-update-pair table) 1 (inc n))
                    ; No dups
                    (g/vector-distinct-by first
                                          (gen-update-pair table)
                                          {:min-elements 1
                                           :max-elements n})))
          where (gen-where? table)]
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
                [2 (flet [n (g/large-integer* {:min -1})]
                         (str " LIMIT " n))]]))

(defn gen-offset?
  "Maybe generates an OFFSET clause."
  []
  (g/frequency [[1 (g/return nil)]
                ; These tables won't get very big, so we want to focus on small
                ; offsets.
                [2 (flet [n (g/large-integer* {:min -1, :max 16})]
                         (str " OFFSET " n))]]))

(defn gen-order?
  "Maybe generates an ORDER BY clause"
  [table]
  (g/frequency
    [[1 (g/return nil)]
     [1 (flet [col (g/elements (:cols table))
               dir (g/elements ["" " ASC" " DESC"])]
              (str " ORDER BY " (:name col) dir))]]))

(defn gen-select-cols
  "Generates the columns to select in a SELECT statement."
  [table]
  (g/one-of [(g/return "*")
             (flet [cols (g/vector (g/elements (:cols table))
                                   1 (+ 1 (count (:cols table))))]
                   (str/join ", " (map :name cols)))
             (g/return "COUNT(*)")]))

(defn gen-select
  "Generates a SELECT statement."
  [schema]
  (->> (g/elements schema)
       (bind (fn [table]
               (flet [cols   (gen-select-cols table)
                      where  (gen-where? table)
                      order  (gen-order? table)
                      limit  (gen-limit?)
                      offset (gen-offset?)]
                     (str "SELECT " cols " FROM " (:name table)
                          where
                          order
                          limit
                          offset))))))

(defn gen-delete
  "Generates a DELETE statement"
  [schema]
  (->> (g/elements schema)
       (bind (fn [table]
               (flet [where (gen-where? table)]
                     (str "DELETE FROM " (:name table) where))))))

(defn gen-statement
  "Generates a statement for a schema."
  [schema]
  (g/frequency [[8  (gen-select schema)]
                [8  (gen-insert schema)]
                [4  (gen-update schema)]
                [1  (gen-delete schema)]]))

(defn id-gen
  "A generator which draws unique IDs from an atom containing an integer. This
  is a stupid hack but I don't know how else to do it."
  [id-atom]
  (g/fmap (fn [_]
            (swap! id-atom inc))
    (g/return nil)))

(defn gen-case
  "Generates a test case for a unique integer ID, drawn from `id-atom`. A case
  is a map of:

      :id           A unique integer id
      :sql          A vector of SQL statements
      :teardown-sql A vector of SQL statements to tear down what this made.

  Options are:

      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts id-atom]
  (g/let [id          (id-gen id-atom)
          schema      (gen-schema (assoc opts :id (swap! id-atom inc)))
          statements  (g/vector (gen-statement schema) 1 1024)]
    {:id id
     :sql (into (schema->create-sql schema)
                statements)
     :teardown-sql (schema->teardown-sql schema)}))

(defn qc!
  "Runs quick-check, drawing IDs from the `last-id` atom, with the given
  options passed to `gen-case`. Returns quickcheck output."
  [n opts queue id- last-id]
  (let [checked
        (tc/quick-check
          n
          (prop/for-all [case (gen-case opts last-id)]
                        (let [result  (promise)
                              id      (:id case)
                              id      (if (= id @id-)
                                        (swap! last-id inc)
                                        id)
                              _       (vreset! id- id)
                              case    (assoc case
                                             :id     id
                                             :final? false
                                             :result result)]
                          ; Send case to generator
                          (swap! queue conj case)
                          ; Is it interesting?
                          (not (= :interesting (:status @result))))))]
    (info "Test.check pass completed:\n"
          (with-out-str (pprint checked)))
    checked))

(defn worker
  "Starts a test.check worker thread. Draws unique IDs from last-id, an atom
  holding an integer. Sends cases to the given atom holding a PersistentQueue.
  Options are:

      :test-cases The number of test cases to run
      :seed       The random seed"
  [test-opts last-id queue]
  (future
    (util/with-thread-name "jepsen test.check"
      (try
        (info "Starting test.check")
        (let [; Ugh I cannot for the life of me figure out how to thread a
              ; unique ID reliably through test.check's machinery. This is an
              ; overlapping hack to make SURE we get unique IDs.
              id- (volatile! nil)]
          ; We are *very* bad at shrinking schemas (for the usual test.check
          ; bind reasons), so we build a loop that gradually expands the
          ; schemas over time.
          (loop [max-table-count 1
                 max-column-count 1]
            (info "Trying up to"
                  max-table-count "tables and"
                  max-column-count "columns")
            (let [checked (qc! (:test-cases test-opts)
                               {:max-table-count  max-table-count
                                :max-column-count max-column-count}
                               queue
                               id-
                               last-id)]
              (if (:pass? checked)
                (cond ; Start off by adding a column
                      (= 1 max-column-count)
                      (recur max-table-count (inc max-column-count))

                      ; When we get to n columns and n tables, start over
                      ; with more tables.
                      (= max-table-count max-column-count)
                      (recur (inc max-table-count) 1))
                ; Done
                checked)))
          ; Re-run with the interesting case.
          ;(when-let [case (-> checked :shrunk :smallest first)]
          ;  (let [result (promise)]
          ;    (swap! queue conj
          ;           (assoc case
          ;                  :result result
          ;                  :final? true))
          ;    @result))
          ; And done
          (swap! queue conj :stop))
        (catch Throwable t
          (warn t "Jepsen test.check worker crashed!")
          (throw t))))))
