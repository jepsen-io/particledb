(ns jepsen.particledb.workload.compare
  "For this workload, we stand up a Postgres and a ParticleDB, generate random
  queries, and compare their results."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [clojure.tools.logging :refer [info warn]]
            [dom-top.core :refer [loopr]]
            [hegel-clj [core :as hc]
                       [generator :as g]]
            [jepsen [control :as c]
                    [checker :as checker]
                    [client :as client]
                    [generator :as gen]
                    [history :as h]
                    [random :as rand]
                    [sql :as sql]
                    [store :as store]
                    [util :as util]]
            [jepsen.sql.client :as sqlc]
            [jepsen.particledb.pdb-client :as pdbc]
            [jepsen.postgres.client :as pgc]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs])
  (:import (clojure.lang PersistentQueue)
           (org.postgresql.util PSQLException)))

(defn execute!
  "Like jdbc execute!, but converts exceptions to values. We want this because
  we're going to generate all kinds of illegal SQL."
  [conn sql]
  (sqlc/try+
    ; Note that Postgres returns qualified names, ParticleDB returns
    ; unqualified ones. We'll unqualify all here.
    (j/execute! conn sql {:builder-fn rs/as-unqualified-lower-maps})
    (catch [:type :connection-closed] e
      ; We actually want this to bubble up, so we can reconnect.
      (throw+ e))
    (catch (:type %) e
      e)
    (catch PSQLException e
      {:type :psql-error, :msg (.getMessage e)})))

(defn equiv-row?
  "Are the given postgres and pdb rows equivalent?"
  [pg pdb]
  ; PG likes to return unknown selects as :?column?, whereas pdb makes up a
  ; name for it like :true or :3.
  (if (= 1 (count pg) (count pdb))
    (if (= :?column? (key (first pg)))
      ; Let this pass if the values are the same
      (= (val (first pg)) (val (first pdb)))
      (= pg pdb))
    (= pg pdb)))

(defn equiv?
  "Are the postgres and particledb results for the given SQL query equivalent?
  Takes a map of :sql (a string) and the :pg and :pdb results from j/execute!."
  [{:keys [sql pg pdb]}]
  (or ; Error maps
      (and (map? pg) (map? pdb) (= (:type pg) (:type pdb)))
      ; Actual data
      (and (= (count pg) (count pdb))
           (every? identity
                   (map equiv-row? pg pdb)))))

(defn origin
  "Takes an SQL operation and returns an origin string describing roughly where
  the failure happened. We use this to discriminate between different kinds of
  weird things ParticleDB does."
  [mops]
  (loopr []
         [{:keys [sql pg pdb] :as mop} mops]
         (if (equiv? mop)
           (recur)
           ; Found a diff
           (do ;(info :diff (pr-str pg) (pr-str pdb))
               (cond
                 ; PG threw an error
                 (and (map? pg) (:msg pg))
                 (condp re-find (:msg pg)
                   ; We know about this: postgres won't let you have more than
                   ; one primary key on a table
                   #"multiple primary keys for table"
                   "multiple primary keys for table"

                   ; You shouldn't assign a column twice, but ParticleDB
                   ; doesn't mind.
                   #"multiple assignments to same column"
                   "multiple assignments to same column"

                   ; You can order by something that's not in the agg fun
                   #"column .+ must appear in the GROUP BY clause"
                   "column must appear in group by"

                   ; I... what... how even...
                   #"operator does not exist"
                   "operator does not exist"

                   ; What is type safety for, really?
                   #"is of type .+"
                   "wrong type"

                   ; `WHERE a` is apparently legal, even if a is an int.
                   #"must be type"
                   "must be type"

                   ; ParticleDB doesn't do NOT NULL
                   #"violates not-null constraint"
                   "not-null constraint"

                   ; Or duplicate constraints
                   #"violates unique constraint"
                   "unique constraint"

                   ; Want to ask if 0 = ':'? Sure, why not
                   #"invalid input syntax"
                   "invalid input syntax"

                   ; You can select nonexistent columns!
                   #"column .+ does not exist"
                   "column does not exist"

                   ; And put giant floats into tiny integer fields
                   #"out of range"
                   "out of range"

                   "unknown")

                 ; PDB threw an error
                 (and (map? pdb) (:msg pdb))
                 (condp re-find (:msg pdb)
                   ; ParticleDB thinks WHERE NULL = TRUE isn't OK, but only
                   ; sometimes`
                   #"filter predicate must produce boolean"
                   "filter predicate must produce boolean"

                   ; Weirdly ParticleDB isn't OK comparing Boolean with Null...
                   ; sometimes, even when PG is fine. Weird.
                   #"Invalid comparison operation"
                   "invalid comparison"

                   ; I guess you can't store infinity???
                   #"Bad value for type double"
                   "bad value for type double"

                   ; Oh NOW you care about integer sizes!
                   #"invalid integer"
                   "invalid integer"

                   ; Oh this is neat. 'Infinity' and 0.0 are... considered
                   ; duplicates for a unique constraint? This is hilarious
                   ; given ParticleDB seems totally incapable of doing
                   ; uniqueness on integers.
                   #"duplicate key value"
                   "duplicate key value"

                   "unknown")

                 ; We got results, but... they differed
                 (and (vector? pg) (vector? pdb))
                 (cond (not= (count pg) (count pdb))
                       "different number of rows"

                       (not= (reduce set/union #{} (map (comp set keys) pg))
                             (reduce set/union #{} (map (comp set keys) pdb)))
                       "different columns"

                       (< 1 (count pg))
                       "different data"

                       (and (= [:next.jdbc/update-count]
                               (keys (first pg))
                               (keys (first pdb))))
                       "different update count"

                       true
                       "different data")

                 ; Not sure what this is
                 true
                 "unknown")))))

; Our client is fairly simple: it takes a vector of SQL strings, runs them on
; both clients, and returns vectors of their results.
(defrecord Client [pg pdb]
  client/Client
  (open! [this test node]
    (assoc this
           :pg  (pgc/open {:postgres-user "postgres"
                           :postgres-password "pw"}
                          node)
           ; I'm going to use the PG client to talk to both, just on the theory
           ; that it'll make the error handling a little less hairy
           :pdb (pgc/open test node)))

  (setup! [this test])

  (invoke! [this test op]
    (binding [sqlc/*error-fn* pgc/error-fn]
      (let [test-case (:value op)
            sql' (mapv (fn [sql]
                         {:sql sql
                          :pg  (execute! pg [sql])
                          :pdb (execute! pdb [sql])})
                       (:sql test-case))
            status (if (every? equiv? sql')
                     :valid
                     :interesting)
            ; What kind of bug is this?
            origin (when (= :interesting status)
                     (origin sql'))
            ; We only want those matched by --compare-origin
            status (if (and (= :interesting status)
                            (or (nil? origin)
                                (re-find (:compare-origin test) origin)))
                     :interesting
                     :valid)]
        ; Try to politely clean up
        (if (= status :valid)
          (doseq [sql (:teardown-sql test-case)]
            (execute! pg [sql])
            (execute! pdb [sql])))
        (assoc op
               :type :ok
               :value (cond-> (assoc (:value op)
                                     :sql    sql'
                                     :status status)
                        origin (assoc :origin origin))))))

  (teardown! [this test])

  (close! [this test]
    (sqlc/close-conn! pg)
    (sqlc/close-conn! pdb)))

; A case is a map of:
;
; {:id        A unique integer ID for this case
;  :sql       A vector of SQL strings to execute
;  :teardown-sql A vector of SQL strings to tear down the state
;  :result    A promise which will receive the status of the case:
;            :interesting, :invalid, :or :valid
;  }

(defrecord HegelGen [; A mutable queue of test cases Hegel wants us to try. An
                     ; atom to a PersistentQueue.
                     queue
                     ; A map of case id -> result-promise. As operations
                     ; complete, we deliver their results to these promises.
                     results]
  gen/Generator
  (op [this test ctx]
    (if-let [test-case (peek @queue)]
      (do (if (= :stop test-case)
            ; That's the end!
            (do nil)
            [(gen/fill-in-op
               {:f     :sql
                :value (dissoc test-case :result)}
               ctx)
             this]))
      ; No case yet
      [:pending this]))

  (update [this test ctx event]
    (if (= :sql (:f event))
      (case (:type event)
        ; We should be consuming the case at the head of the queue, in which
        ; case we need to go in and mutate it. We're taking advantage of the
        ; fact that the generator itself is single-threaded here.
        :invoke
        (let [op-case    (:value event)
              queue-case (peek @queue)]
          (assert (= (:id op-case) (:id queue-case)))
          ; Safe because we're single-threaded and the sole consumer
          (swap! queue pop)
          (assoc this :results
                 (assoc results (:id queue-case) (:result queue-case))))

        ; When we complete an sql event, we'll send its result back to Hegel.
        :ok
        (let [id     (:id (:value event))
              result (get results id)
              _ (assert result)
              status (:status (:value event))
              _ (assert (keyword? status))]
          ; Send back to Hegel
          (deliver result (select-keys (:value event) [:status :origin]))
          ; And we don't need to track that result any more
          (assoc this :results (dissoc results id)))

        ; Infos we'll send back as invalid, I guess
        :info 
        (let [id (:id (:value event))
              result (get results id)]
          (deliver result {:status :invalid})
          (assoc this :results (dissoc results id)))

        :fail this)

      ; Not an sql op
      this)))


(defn hegel-gen
  "A generator that draws operations from Hegel. Takes an atom with a
  PersistentQueue of test cases."
  [queue]
  (HegelGen. queue {}))

;; SQL generators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gen-branch
  "Evaluates one of n branches based on Hegel."
  [& branches]
  (let [n (count branches)]
    (case n
      0 nil
      1 (first branches)
      `(case (hc/gen (g/integer 0 ~(dec n)))
         ~@(mapcat identity (map-indexed vector branches))))))

(defn gen-nth
  "Picks a random element of the given vector."
  [xs]
  (g/let [i (g/integer 0 (dec (count xs)))]
    (nth xs i)))

(def column-names
  "What we call columns"
  ["a" "b" "c" "d" "e"])

(def table-names
  "What we call tables"
  ["t" "u" "v"])

(def gen-column-types
  "A schema to generate column types."
  (g/one-of
    (g/constant :smallint) ; 2 bytes
    (g/constant :bigint)   ; 8 bytes
    (g/constant :float4)
    (g/constant :boolean)
    (g/constant :text)))

(def all-types
  "All possible types."
  #{:smallint :bigint :float4 :boolean :text})

(def num-types
  "All numeric types"
  #{:smallint :bigint :float4})

(defn general-type
  "Generalize a type up to a set of compatible types."
  [type]
  (case type
    :smallint num-types
    :bigint   num-types
    :int      num-types
    :float4   num-types
    type))

(def gen-boolean-lit
  "A schema for boolean literals"
  (g/one-of (g/constant "TRUE")
            (g/constant "FALSE")
            (g/constant "NULL")))

(def gen-string-lit
  "A schema for string literals. We're not going to go crazy here--I assume
  there are parser bugs galore to be found with weird strings in ParticleDB,
  but I feel like escaping might be more work than it's worth right now."
  (g/fmap (fn [s] (str "'" s "'"))
          (g/regex #"[A-Za-z0-9\- _]+"
                   {:full-match? true})))

; ParticleDB's integers are... weird. I think internally it maps everything to
; an int64. You can happily assign larger numbers to any int type, including a
; smallint, and it will clip them to 9223372036854775807, which is the biggest
; 64-bit signed int.
(def gen-int-lit
  "A schema for integer literals."
  ; These are one bigger than the actual integer bounds
  (g/integer -2147483649 2147483648))

(def gen-smallint-lit
  "A schema for 2-byte int literals."
  ; These are one bigger than the actual bounds
  (g/integer -32769 32768))

(def gen-bigint-lit
  "A schema for 8-byte int literals."
  ; These are one bigger than the actual bounds
  (g/integer -9223372036854775809 9223372036854775808))

(def gen-float-lit
  "A schema for float literals."
  (g/fmap (fn [f]
            (cond
              (NaN? f)      "'NaN'"
              (infinite? f) (str "'" (str f) "'")
              true          (str f)))
          (g/float {:width 32})))

(defn gen-lit-
  "Generates a literal value for a column schema, based on its type."
  [type]
  (if (set? type)
    (recur (gen-nth (vec type)))
    (hc/gen
      (case type
        :int      gen-int-lit
        :smallint gen-smallint-lit
        :bigint   gen-bigint-lit
        :float4   gen-float-lit
        :text     gen-string-lit
        :boolean  gen-boolean-lit
        ))))

(defn gen-lit
  "Generates a possibly NULL literal value of the given type."
  [type]
  (g/let [null? false]; (g/boolean)]
    (if null?
      "NULL"
      (gen-lit- type))))

(defn gen-expr-leaf
  "Generates a simple expression of the given type which is valid in the given
  table. For instance, 2, 'foo', or a, assuming a is a column of the right
  type."
  [table type]
  (gen-branch
    ; Column
    (->> (:cols table)
         (filter (if (keyword? type)
                   ; Direct keyword match
                   (comp #{type} :type)
                   ; Predicate
                   (comp type :type)))
         (map (comp g/constant :name))
         (apply g/one-of)
         hc/gen)
    ; Literal
    (gen-lit type)
    ; Sometimes, make up a random name just for grins. I can't believe this
    ; actually works in ParticleDB, but it does. We choose a-f so that we
    ; *mostly* generate real column names, but sometimes of the wrong type, and
    ; sometimes ones that don't exist.
    (hc/gen (g/regex "[a-f]" {:full-match? true}))))

(defn gen-type
  "Generates a type thats in the given table."
  [table]
  (->> (:cols table)
       (map :type)
       distinct
       (map g/constant)
       (apply g/one-of)
       (g/fmap general-type)
       hc/gen))

;; Expression generating

(declare gen-expr)

;; Numeric expressions

(declare gen-expr-num)

(defn gen-expr-num-binary
  "Generates a binary numeric expression, like (1 + b)"
  [table]
  (g/let [a  (gen-expr-leaf table num-types)
          b  (gen-expr-leaf table num-types)
          op (g/one-of "+" "-" "*" "/" "%" "^")]
    (str "(" a " " op " " b ")")))

(defn gen-expr-num
  "Generates a numeric expression, like 4, a, or (a - b)"
  [table]
  (gen-branch
    (gen-expr-leaf table num-types)
    (gen-expr-num-binary table)))

;; Boolean expressions

(declare gen-expr-boolean)

(def boolean-comparators
  "A schema for boolean comparators like <, =, !=, ..."
  (apply g/one-of
         (mapv g/constant
               ["=" "!=" "<" ">" "<=" ">="])))

(defn gen-expr-boolean-null
  "Generates a boolean expression which checks for null."
  [table]
  (g/let [not? (g/boolean)]
    (str "(" (gen-expr table all-types) " IS " (when not? "NOT ")
         "NULL)")))

(defn gen-expr-boolean-comp
  "Generates a boolean expression which compares things."
  [table]
  (g/let [same-type? (g/boolean)
          type       (gen-type table)
          a          (gen-expr table type)
          b          (gen-expr table (if same-type?
                                       type
                                       (gen-type table)))
          op boolean-comparators]
    (str "(" a " " op " " b ")")))

(defn gen-expr-boolean-branch
  "Generates a compound boolean expression using AND/OR/NOT"
  [table]
  (gen-branch
    (str "NOT " (gen-expr-boolean table))
    (str "(" (gen-expr-boolean table) " OR " (gen-expr-boolean table) ")")
    (str "(" (gen-expr-boolean table) " AND " (gen-expr-boolean table) ")")))

(defn gen-expr-boolean
  "Generates a boolean expression, either leaf or branch."
  [table]
  (gen-branch
    (gen-expr-leaf table :boolean)
    (gen-expr-boolean-comp table)
    (gen-expr-boolean-null table)
    (gen-expr-boolean-branch table)))

(defn gen-expr
  "Generates an expression of the given type in the table."
  [table type]
  (case type
    :boolean  (gen-expr-boolean table)
    :int      (gen-expr-num table)
    :float    (gen-expr-num table)
    num-types (gen-expr-num table)
    (gen-expr-leaf table type)))


(defn gen-column-schema
  "Generates a column schema. A column schema is a map of the form:

  {:name         \"c1\"
   :type         :int
   :not-null?    false
   :unique?      false
   :primary-key? true}"
  [name]
  (g/let [type         gen-column-types
          not-null?    (g/boolean)
          unique?      (g/boolean)
          primary-key? (g/boolean)]
    (cond-> {:name name
             :type type}
      not-null?    (assoc :not-null? true)
      unique?      (assoc :unique? true)
      primary-key? (assoc :primary-key? true))))

(defn column-schema->sql
  "Turns a column schema into the SQL to create it."
  [{:keys [type primary-key? unique? not-null?] :as col}]
  (str (:name col) " " (name type)
       (when not-null? " NOT NULL")
       (when unique? " UNIQUE")
       (when primary-key? " PRIMARY KEY")))

(defn gen-table-schema
  "Generates a table schema. A table schema is a map of the form:

  {:name \"c1\"
   :cols [column-schema ...]}"
  [name]
  (hc/with-span
    {:name name
     :cols (first
             (g/collect [[cols i pk?] [[] 0 false]
                         {:min-size 1, :max-size 4}]
                        (let [col (gen-column-schema (column-names i))]
                          (if (and pk? (:primary-key? col))
                            ; ParticleDB is happy to let us do several primary
                            ; keys, but that's basically ALL the errors. When
                            ; they're more robust, come back and try this again.
                            :hegel-clj/reject
                            [(conj cols col)
                             (inc i)
                             (or pk? (:primary-key? col))]))))}))

(defn table-schema->sql
  "Turns a table schema into the SQL to create it."
  [{:keys [name cols]}]
  (str "CREATE TABLE " name " ("
       (str/join ", " (map column-schema->sql cols)) ")"))

(defn gen-schema
  "Generates a schema. A schema is a vector of table schemas."
  [id]
  (second
    (g/collect [[i tables] [0, []]
                {:min-size 1, :max-size 3}]
               ; So our table names are always unique
               [(inc i)
                (conj tables
                      (gen-table-schema (str (table-names i) "_" id)))])))

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

(defn gen-insert
  "Generates an INSERT statement."
  [schema]
  (g/let [table (gen-nth schema)
          cols  (g/shuffle (g/constant (:cols table)))]
    (str "INSERT INTO " (:name table)
         " (" (str/join ", " (map :name cols)) ")"
         " VALUES (" (str/join ", " (mapv (comp gen-lit :type) cols)) ")")))


(defn gen-where?
  "Maybe generates a WHERE clause"
  [table]
  (gen-branch
    ""
    (str " WHERE " (gen-expr-boolean table))))

(defn gen-update
  "Generates an UPDATE statement."
  [schema]
  (let [table   (gen-nth schema)
        cols    (:cols table)
        col-gen (apply g/one-of (mapv g/constant (:cols table)))]
    (g/let [; ParticleDB doesn't mind setting a = 1, a = 2, but PG does, and
            ; that's noisy.
            col-is (g/vector {:unique? true
                              :min-size 1
                              :max-size (count cols)}
                             (g/integer 0 (dec (count cols))))
            cols   (map cols col-is)
            where? (gen-where? table)]
      (str "UPDATE " (:name table) " SET "
           (->> cols
                (mapv (fn [col]
                        (let [v (gen-branch
                                  ; Either a literal
                                  (gen-lit (:type col))
                                  ; Or another column in the table
                                  (:name (hc/gen col-gen)))]
                          (str (:name col) " = " v))))
                (str/join ", "))
           where?))))

(defn gen-limit?
  "Maybe generates a LIMIT clause"
  []
  (g/fmap (fn [[limit? i]]
            (when limit?
              (str " LIMIT " i)))
          (g/tuple (g/boolean)
                   (g/integer 0 1024))))

(defn gen-offset?
  "Maybe generate an OFFSET clause"
  []
  (g/fmap (fn [[offset? i]]
            (when offset?
              (str " OFFSET " i)))
          (g/tuple (g/boolean)
                   (g/integer 0 16))))

(defn gen-order?
  "Maybe generates an ORDER BY clause"
  [table]
  (g/fmap (fn [[order? i dir]]
            (when order?
              (str " ORDER BY " (:name (nth (:cols table) i)) " "
                   (if dir "ASC" "DESC"))))
          (g/tuple (g/boolean)
                   (g/integer {:min 0, :max (dec (count (:cols table)))})
                   (g/boolean))))

(defn gen-select-cols
  "Generates the list of columns to select from a table, like 'a, b' or '*'"
  [table]
  (let [cols (:cols table)]
     (g/one-of
       (g/constant "*")
       (->> (g/vector {:min-size 1, :max-size 4}
                      (g/integer 0 (dec (count cols))))
            (g/fmap (fn [is]
                      (->> is
                           (map (comp :name cols))
                           (str/join ", ")))))
       (g/constant "COUNT(*)"))))

(defn gen-select
  "Generates a select statement."
  [schema]
  (g/let [table  (gen-nth schema)
          cols   (gen-select-cols table)
          where  (gen-where? table)
          order  (gen-order? table)
          limit  (gen-limit?)
          offset (gen-offset?)]
    (str "SELECT " cols " FROM " (:name table)
         where
         order
         limit
         offset)))

(defn gen-delete
  "Generates a DELETE statement."
  [schema]
  (g/let [table (gen-nth schema)
          where (gen-where? table)]
    (str "DELETE FROM " (:name table) where)))

(defn gen-statement
  "Generates an SQL statement for a schema."
  [schema]
  ((gen-nth [gen-select gen-insert gen-update gen-delete])
   schema))

(defn gen-case
  "Generates a test case for a unique integer ID. A case is a map of:

  :id           A unique integer id
  :sql          A vector of SQL statements
  :teardown-sql A vector of SQL statements to tear down what this made."
  [id]
  (let [schema (gen-schema id)]
    {:id  id
     :sql (g/collect [sqls (schema->create-sql schema)
                      {:min-size 1 :max-size 128}]
                      (conj sqls (gen-statement schema)))
     :teardown-sql (schema->teardown-sql schema)}))

;; Debugging

(defn sample-cases
  "Generates a few random cases with Hegel. Helpful for debugging at the repl."
  []
  (hc/sample 10 (gen-case 0)))

(defn sample-exprs
  "Generates a few random expressions with Hegel. Helpful for debugging."
  []
  (hc/sample 10
             (let [schema (gen-schema 0)
                   table  (gen-nth schema)]
               [table (gen-expr-boolean-comp table)])))

;; Checker

(defn indented-pprint-str
  "Prints to a string, with the given indent string first."
  [indent x]
  (-> (str indent (with-out-str (pprint x)))
      (str/replace "\n" (str "\n" indent))
      (str/replace #"[\n\s]*$" "")))

(defn condense-sql
  "Condenses a single SQL statement; see condense-case."
  [sql-str]
  (str/replace sql-str #" ([tuv])_\d+ " " $1 "))

(defn condense-op
  "Rewrites an operation so that it is easier to understand and explore
  interactively. Rewrites table names (e.g. 't_123') as simply 't'."
  [op]
  (update-in op [:value :sql]
             (fn [sql]
               (mapv #(update % :sql condense-sql) sql))))

(defn write-case!
  "Writes a test case op to a file."
  [test dir {:keys [value] :as op}]
  (let [{:keys [id origin sql]} value]
    (store/with-out-file test (str dir "/" (when origin (str origin " "))
                                   "/" origin
                                   id ".txt")
      ; First, step-by-step with results.
      (doseq [{:keys [sql pg pdb] :as mop} sql]
        (print sql)
        (println ";")
        (if (equiv? mop)
          (println (indented-pprint-str "  " pdb))
          (do ; Oh, they're different
              (println "  ParticleDB:")
              (println (indented-pprint-str "    " pdb))
              (println "  Postgres:")
              (println (indented-pprint-str "    " pg))
              (println))))

      ; Then condensed case
      (println)
      (println "# Condensed case (for reproducing)")
      (println)
      (doseq [{:keys [sql]} (:sql (:value (condense-op op)))]
        (print sql)
        (println ";")))))

(defrecord Checker []
  checker/Checker
  (check [this test history opts]
    (let [final (->> history
                     (h/filter h/ok?)
                     (h/filter (comp :final? :value))
                     (into []))
          interesting (->> history
                           (h/filter h/ok?)
                           (h/filter (fn [{:keys [value] :as op}]
                                       (= :interesting (:status value))))
                           (into []))]
      (doseq [op final]
        (write-case! test "final" op))
      (doseq [op interesting]
        (write-case! test "interesting" op))

      {:valid? (not (seq interesting))
       :interesting interesting
       :final final})))

;; Plumbing

(defn hegel-worker
  "Starts a Hegel test in a future. Draws unique IDs from last-id, an atom to
  an integer. Sends cases to the given atom with a PersistentQueue."
  [test-opts last-id queue]
  (future
    (util/with-thread-name "jepsen hegel"
      (loop []
        ; Keep hunting until we find new bugs
        (let [res (hc/run-test! test-opts
                                (g/let [id        (swap! last-id inc)
                                        result    (promise)
                                        test-case (gen-case id)]
                                  ; Send case to generator.
                                  (swap! queue conj (assoc test-case
                                                           :final? (hc/final?)
                                                           :result result))
                                  ; And wait for Jepsen to evaluate it.
                                  @result))]
          (info "Hegel complete:" (with-out-str (pprint res)))
          (if false ;(every? #(not= "unknown" (:origin %)) (:final res))
            (do (info "Trying again to see if we can find new bugs")
                (recur))
            ; Done
            (swap! queue conj :stop)))))))


(defn workload
  "Constructs a workload given CLI options."
  [opts]
  (let [last-id (atom -1)
        queue   (atom PersistentQueue/EMPTY)
        hegel-worker (hegel-worker
                       {:test-cases 100
                        :seed (rand/long)}
                       last-id
                       queue)]
    {:client    (Client. nil nil)
     :generator (hegel-gen queue)
     :checker   (Checker.)}))
