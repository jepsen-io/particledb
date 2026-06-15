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
            [jepsen.particledb.gen.tc :as gen.tc]
            [jepsen.postgres.client :as pgc]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs])
  (:import (clojure.lang Keyword
                         MapEntry
                         PersistentArrayMap
                         PersistentVector
                         PersistentQueue)
           (java.sql ResultSet
                     ResultSetMetaData)
           (org.postgresql.geometric PGpoint)
           (org.postgresql.util PSQLException)))

; We use Clojure representations for some of the more interesting datatypes;
; makes them easier to print and serialize.
(defrecord Point [x y])

(def builder-fn
  "This is how we rewrite next.jdbc rows before further processing."
  (rs/as-maps-adapter
    ; Note that Postgres includes table names in column identifiers, but
    ; ParticleDB doesn't. We'll be nice and just drop the table names.
    ; ParticleDB also returns capital columns like COUNT, iwhere PG says count.
    rs/as-unqualified-lower-maps
    (fn reader [^ResultSet rs ^ResultSetMetaData _ ^Integer i]
      (let [v (.getObject rs i)]
        ; (info "builder type" (class v))
        (condp instance? v
          PGpoint (Point. (.x v) (.y v))
          v)))))

(defn execute!
  "Like jdbc execute!, but converts exceptions to values. We want this because
  we're going to generate all kinds of illegal SQL."
  [conn sql]
  (sqlc/try+
    ; Note that Postgres returns qualified names, ParticleDB returns
    ; unqualified ones. We'll unqualify all here.
    (j/execute! conn sql {:builder-fn builder-fn})
    (catch [:type :connection-closed] e
      ; We actually want this to bubble up, so we can reconnect.
      (throw+ e))
    (catch (:type %) e
      e)
    (catch PSQLException e
      {:type :psql-error, :msg (.getMessage e)})
    (catch IllegalStateException e
      {:type :illegal-state-exception, :msg (.getMessage e)})))

(defn equiv-double?
  "Floating-point equivalence. We allow a small difference between results, and
  consider NaNs equal since we want both DBs to return NaN!"
  [^double a ^double b]
  (cond ; This is one of those places where NaNs *are* equivalent!
        (and (NaN? a) (NaN? b))
        true

        (or (NaN? a) (NaN? b))
        false

        (or (infinite? a) (infinite? b))
        (= a b)

        ; So our later epsilon comparison won't blow up
        (and (== 0.0 a b))
        true

        ; Otherwise, equality to within two units-in-last-place
        true
        (let [; How big is the bigger one?
              scale (Math/max (Math/abs a) (Math/abs b))
              ; Unit in last place
              ulp (Math/ulp scale)]
          ; Sigh. This is the *precise* way to do it, but ParticleDB stores
          ; reals as 64-bit floats, so, uh... let's let things be extremely
          ; sloppy.
          (<= (Math/abs (- a b))
              (* ulp 2))
          ; Much looser
          (<= (/ (Math/abs (- a b)) scale)
              1/1024)
              )))


(defn equiv?
  "Are the given values equivalent?"
  [a b]
  (let [ca (class a)
        cb (class b)]
    (if (identical? a b)
      true

      (if (and (not (identical? ca cb))
               (not (and (float? a)   (float? b)))
               (not (and (integer? a) (integer? b))))
        ; Way different classes!
        false

        ; OK, at least comparable classes
        (condp instance? a
          Boolean
          (= a b)

          Keyword
          (identical? a b)

          Integer
          (= a b)

          Long
          (= a b)

          Float
          (equiv-double? a b)

          Double
          (equiv-double? a b)

          String
          (= a b)

          MapEntry
          (and (= (key a) (key b))
               (equiv? (val a) (val b)))

          PersistentArrayMap
          (and (= (count a) (count b))
               ; Eh, not efficient but whatevs
               (loopr []
                      [k (distinct (concat (keys a) (keys b)))]
                      (if (equiv? (get a k ::not-found) (get b k ::not-found))
                        (recur)
                        false)
                      true))

          (throw+ {:type   :no-equivalence
                   :a      a
                   :b      b
                   :class-a (symbol (.getName (class a)))
                   :class-b (symbol (.getName (class b)))}))))))

(defn equiv-row?
  "Are the given postgres and pdb rows equivalent?"
  [pg pdb]
  ; PG likes to return unknown selects as :?column?, whereas pdb makes up a
  ; name for it like :true or :3.
  (if (= 1 (count pg) (count pdb))
    (if (= :?column? (key (first pg)))
      ; Let this pass if the values are the same
      (equiv? (val (first pg)) (val (first pdb)))
      (equiv? pg pdb))
    (equiv? pg pdb)))

(defn equiv-unordered-row
  "Turns a row into something we can compare using =, for equiv-unordered?"
  [row]
  (update-vals row
               (fn project [x]
                 (cond ; Being SUPER conservative
                       (float? x) (Math/round x)

                       true   x))))

(defn equiv-unordered-set
  "Computes the unordered, comparable-with-=, set for equiv-unordered?"
  [rows]
  (if (sequential? rows)
    (persistent!
      (reduce (fn [rows row]
                (conj! rows (equiv-unordered-row row)))
              (transient #{})
              rows))
    ; Oh, uhhh, maybe this is an error instead?
    rows))

(defn equiv-unordered?
  "When we do a select without an ORDER BY clause, we have to allow the results
  to be equivalent regardless of row order. This is a little tricky because we
  have all this weird logic around floating point equivalence, on account of
  ParticleDB doing higher floating-point precision. So what we do here is cheat
  a little--we transform the rows into things we know will DEFINITELY compare
  equal (like integers), and check for set equivalence."
  [as bs]
  (= (equiv-unordered-set as)
     (equiv-unordered-set bs)))

(defn unordered-select?
  "Is this SQL string a SELECT without an ORDER BY clause?"
  [sql]
  (and (re-find #"^SELECT" sql)
       ; Ugh, this is fragile, but it's probably not going to break given how
       ; weird our generated strings are.
       (not (re-find #"ORDER BY" sql))))

(defn equiv-mop?
  "Are the postgres and particledb results for the given SQL query equivalent?
  Takes a map of :sql (a string) and the :pg and :pdb results from j/execute!."
  [{:keys [sql pg pdb]}]
  (or ; Error maps
      (and (map? pg) (map? pdb) (= (:type pg) (:type pdb)))
      ; On commit of an aborted transaction, PG returns {:update-count 0}, and
      ; ParticleDB returns an abort error. Not sure why, but let's let that
      ; slide for now.
      (and (map? pdb)
           (= :txn-aborted (:type pdb))
           (= [{:next.jdbc/update-count 0}] pg))

      ; Actual data
      (and (= (count pg) (count pdb))
           (cond ; If we're doing an unordered select, compare as sets
                 (unordered-select? sql)
                 (equiv-unordered? pg pdb)

                 ; Otherwise we should get equivalent rows
                 true
                 (every? identity
                         (map equiv-row? pg pdb))))))

(defn origin
  "Takes an SQL operation and returns an origin string describing roughly where
  the failure happened. We use this to discriminate between different kinds of
  weird things ParticleDB does."
  [mops]
  (loopr []
         [{:keys [sql pg pdb] :as mop} mops]
         (if (equiv-mop? mop)
           (recur)
           ; Found a diff
           (do (info :diff (pr-str pg) (pr-str pdb))
               (cond
                 ; PG threw an error
                 (and (map? pg) (:msg pg))
                 (condp re-find (:msg pg)
                   ; You can order by something that's not in the agg fun
                   ; TODO: repro this again
                   #"column .+ must appear in the GROUP BY clause"
                   "column must appear in group by"

                   ; Or divide by zero
                   #"division by zero"
                   "division by zero"

                   ; Want to ask if 0 = ':'? Sure, why not
                   #"invalid input syntax"
                   "invalid input syntax"

                   ; What is type safety for, really?
                   #"is of type .+"
                   "wrong type"

                   ; We know about this: postgres won't let you have more than
                   ; one primary key on a table
                   #"multiple primary keys for table"
                   "multiple primary keys for table"

                   ; You shouldn't assign a column twice, but ParticleDB
                   ; doesn't mind.
                   #"multiple assignments to same column"
                   "multiple assignments to same column"

                   ; `WHERE a` is apparently legal, even if a is an int.
                   #"must be type"
                   "must be type"

                   ; I... what... how even...
                   #"operator does not exist"
                   "operator does not exist"

                   ; ParticleDB doesn't do NOT NULL
                   #"violates not-null constraint"
                   "not-null constraint"

                   ; Or duplicate constraints
                   #"violates unique constraint"
                   "unique constraint"

                   ; You can select nonexistent columns!
                   #"column .+ does not exist"
                   "column does not exist"

                   ; And put giant floats into tiny integer fields
                   #"out of range"
                   "out of range"

                   #"operator is not unique"
                   "operator not unique"

                   "unknown postgres error")

                 ; PDB threw an error
                 (and (map? pdb) (:msg pdb))
                 (condp re-find (:msg pdb)
                   ; I guess you can't store infinity???
                   #"Bad value for type double"
                   "bad value for type double"

                   #"could not serialize access"
                   "could not serialize"

                   ; Oh this is neat. All floating point numbers (e.g. 0.0,
                   ; 1.2, and Infinity) are considered duplicates for a unique
                   ; constraint? This is hilarious given ParticleDB seems
                   ; totally incapable of doing uniqueness on integers.
                   ; TODO: write this up
                   #"duplicate key value"
                   "duplicate key value"

                   ; ParticleDB thinks WHERE NULL = TRUE isn't OK, but only
                   ; sometimes`
                   #"filter predicate must produce boolean"
                   "filter predicate must produce boolean"

                   ; Oh *neat*
                   #"internal error"
                   "internal error"

                   ; Weirdly ParticleDB isn't OK comparing Boolean with Null.
                   ; sometimes, even when PG is fine. Weird.
                   #"Invalid comparison operation"
                   "invalid comparison"

                   ; Oh NOW you care about integer sizes!
                   #"invalid integer"
                   "invalid integer"


                   ; I managed to drive PDB into a state once where it
                   ; complained of this for any table mutation.
                   #"non-nullable but contains null values"
                   "non-nullable contains null"

                   #"overflow"
                   "overflow"

                   #"unsupported .+ operator"
                   "unsupported operator"

                   #"underflow"
                   "underflow"

                   "unknown particledb error")

                 ; We got results, but... they differed
                 (and (vector? pg) (vector? pdb))
                 (cond ; TODO: write this up
                       (not= (count pg) (count pdb))
                       "different number of rows"

                       ; TODO: write this up
                       (not= (reduce set/union #{} (map (comp set keys) pg))
                             (reduce set/union #{} (map (comp set keys) pdb)))
                       "different columns"

                       (< 1 (count pg))
                       "different data"

                       ; One or fewer rows; possibly a metadata row like
                       ; update-count
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
(defrecord Client [node pg pdb]
  client/Client
  (open! [this test node]
    (assoc this
           :node node
           :pg  (pgc/open {:postgres-user "postgres"
                           :postgres-password "pw"}
                          node)
           ; I'm going to use the PG client to talk to both, just on the theory
           ; that it'll make the error handling a little less hairy
           :pdb (pgc/open test node)))

  (setup! [this test])

  (invoke! [this test op]
    (binding [sqlc/*error-fn* pgc/error-fn]
      ; I suspect that despite my best efforts, state (BEGIN...COMMIT)? might
      ; be leaking between successive ops on the same client, so let's be EXTRA
      ; careful.
      (let [pg  (pgc/open {:postgres-user "postgres"
                           :postgres-password "pw"}
                          node)
            pdb (pgc/open test node)]
        (try
          (let [test-case (:value op)
                id        (:id test-case)
                sql' (mapv (fn [sql]
                             ; Replace tables like <t> with t_123
                             (let [sql' (gen.tc/mangle-table-str id sql)]
                               {:sql sql
                                :pg  (execute! pg [sql'])
                                :pdb (execute! pdb [sql'])}))
                           (:sql test-case))
                status (if (every? equiv-mop? sql')
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
                            (= :interesting status) (assoc :origin origin))))
          (finally
            (sqlc/close-conn! pg)
            (sqlc/close-conn! pdb))))))

  (teardown! [this test])

  (close! [this test]
    (sqlc/close-conn! pg)
    (sqlc/close-conn! pdb)))

(declare write-case!)

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
          ; If we get an interesting case, write it right now.
          (when (= :interesting status)
            (future (write-case! test
                                {:dir "interesting"
                                 :origin-dir? true}
                                event)))
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
      `(hc/with-span
         (case (hc/gen (g/integer 0 ~(dec n)))
           ~@(->> branches
                  (map-indexed (fn [i form]
                                 [i `(hc/with-span ~form)]))
                  (mapcat identity)))))))

(defn gen-nth
  "Picks a random element of the given vector."
  [xs]
  (hc/with-span
    (g/let [i (g/integer 0 (dec (count xs)))]
      (nth xs i))))

(def column-names
  "What we call columns"
  ["a" "b" "c" "d" "e"])

(def table-names
  "What we call tables"
  ["t" "u" "v"])

(def gen-column-types
  "A schema to generate column types."
  (g/one-of
    (g/constant :boolean)
    (g/constant :smallint) ; 2 bytes
    (g/constant :bigint)   ; 8 bytes
    (g/constant :real)
    (g/constant :double)
    (g/constant :text)))

(def all-types
  "All possible types."
  #{:smallint :bigint :real :double :boolean :text})

(def num-types
  "All numeric types"
  #{:smallint :bigint :real :double})

(defn general-type
  "Generalize a type up to a set of compatible types."
  [type]
  (case type
    :smallint num-types
    :bigint   num-types
    :int      num-types
    :real     num-types
    :double   num-types
    type))

(def gen-boolean-lit
  "A schema for boolean literals"
  (g/one-of (g/constant "TRUE")
            (g/constant "FALSE")))

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

(defn float->str
  "Converts a float to an SQL string."
  [f]
  (cond
    (NaN? f)      "'NaN'"
    (infinite? f) (str "'" (str f) "'")
    true          (str f)))

(def gen-real-lit
  "A schema for `real` float literals."
  (g/fmap float->str (g/float {:width 32})))

(def gen-double-lit
  "A schema for `double` float literals."
  (g/fmap float->str (g/float {:width 64})))

(def gen-absurd-float-lit
  "A schema for a truly ludicrous float."
  (g/constant "1.234E400"))

(defn almost-always
  "A generator which is almost always one thing, but rarely another."
  [common rare]
  (g/bind (fn [f]
            (if (< 0.99 f)
              rare
              common))
          (g/float 0 1)))

(defn gen-lit-
  "Generates a literal value for a column schema, based on its type."
  [type]
  (if (set? type)
    (recur (gen-nth (vec type)))
    (hc/with-span
      (hc/gen
        (case type
          :int      gen-int-lit
          :smallint gen-smallint-lit
          :bigint   gen-bigint-lit
          ; For PG, 32-bit IEEE 754
          :real     (almost-always gen-real-lit gen-absurd-float-lit)
          ; For PG, 64-bit IEEE 754
          :double   (almost-always gen-double-lit gen-absurd-float-lit)
          :text     gen-string-lit
          :boolean  gen-boolean-lit
          )))))

(defn gen-lit
  "Generates a possibly NULL literal value of the given type."
  [type]
  (hc/with-span
    (g/let [nullp (g/float 0 1)]
      (if (< 0.9 nullp)
        "NULL"
        (gen-lit- type)))))

(defn gen-expr-leaf
  "Generates a simple expression of the given type which is valid in the given
  table. For instance, 2, 'foo', or a, assuming a is a column of the right
  type."
  [table type]
  (gen-branch
    ; Literal
    (gen-lit type)
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
    ; Sometimes, make up a random name just for grins. I can't believe this
    ; actually works in ParticleDB, but it does. We choose a-f so that we
    ; *mostly* generate real column names, but sometimes of the wrong type, and
    ; sometimes ones that don't exist.
    (hc/gen (g/regex "[a-f]" {:full-match? true}))))

(defn gen-type
  "Generates a type thats in the given table."
  [table]
  (hc/with-span
    (->> (:cols table)
         (map :type)
         distinct
         (map g/constant)
         (apply g/one-of)
         (g/fmap general-type)
         hc/gen)))

;; Expression generating

(declare gen-expr)

;; Numeric expressions

(declare gen-expr-num)

(defn gen-expr-num-binary
  "Generates a binary numeric expression, like (1 + b)"
  [table]
  (hc/with-span
    (g/let [a  (gen-expr-num table)
            b  (gen-expr-num table)
            op (apply g/one-of (mapv g/constant ["+" "-" "*" "/" "%" "^"]))]
      (str "(" a " " op " " b ")"))))

(defn gen-expr-num
  "Generates a numeric expression, like 4, a, or (a - b)"
  [table]
  (hc/with-span
    (gen-branch
      (gen-expr-leaf table num-types)
      (gen-expr-num-binary table))))

;; Boolean expressions

(declare gen-expr-boolean)

(def binary-comparators
  "A schema for binary comparators like <, =, !=, ..."
  (apply g/one-of
         (mapv g/constant
               ["=" "!=" "<" ">" "<=" ">="])))

(defn gen-expr-boolean-null
  "Generates a boolean expression which checks for null."
  [table]
  (hc/with-span
    (g/let [not? (g/boolean)]
      (str "(" (gen-expr table all-types) " IS " (when not? "NOT ")
           "NULL)"))))

(defn gen-expr-boolean-comp
  "Generates a boolean expression which compares things."
  [table]
  (hc/with-span
    (g/let [same-type? (g/boolean)
            type       (gen-type table)
            a          (gen-expr table type)
            b          (gen-expr table (if same-type?
                                         type
                                         (gen-type table)))
            op binary-comparators]
      (str "(" a " " op " " b ")"))))

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
  (hc/with-span
    (case type
      :boolean  (gen-expr-boolean table)
      :smallint (gen-expr-num table)
      :int      (gen-expr-num table)
      :bigint   (gen-expr-num table)
      :real     (gen-expr-num table)
      :double   (gen-expr-num table)
      num-types (gen-expr-num table)
      (gen-expr-leaf table type))))

(defn gen-column-schema
  "Generates a column schema. A column schema is a map of the form:

  {:name         \"c1\"
   :type         :int
   :not-null?    false
   :unique?      false
   :primary-key? true}"
  [name]
  (hc/with-span
    (g/let [type         gen-column-types
            not-null?    (g/boolean)
            unique?      (g/boolean)
            primary-key? (g/boolean)]
      (cond-> {:name name
               :type type}
        not-null?    (assoc :not-null? true)
        unique?      (assoc :unique? true)
        primary-key? (assoc :primary-key? true)))))

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
                          (if (and pk? (:primary-key? col)
                                   ; ParticleDB is happy to let us do several
                                   ; primary keys, but we don't want to stop
                                   ; there, so we make these errors relatively
                                   ; rare.
                                   (< 0.01 (hc/gen (g/float 0 1))))
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
  (hc/with-span
    (gen-branch
      ""
      (str " WHERE " (gen-expr-boolean table)))))

(defn gen-update
  "Generates an UPDATE statement."
  [schema]
  (hc/with-span
    (let [table   (gen-nth schema)
          cols    (:cols table)
          col-gen (apply g/one-of (mapv g/constant (:cols table)))]
      (g/let [; ParticleDB doesn't mind setting a = 1, a = 2, but PG does, and
              ; that's noisy.
              col-is (g/vector {:unique? (< (hc/gen (g/float 0 1)) 0.01)
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
             where?)))))

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
  (hc/with-span
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
           offset))))

(defn gen-delete
  "Generates a DELETE statement."
  [schema]
  (hc/with-span
    (g/let [table (gen-nth schema)
            where (gen-where? table)]
      (str "DELETE FROM " (:name table) where))))

(defn gen-statement
  "Generates an SQL statement for a schema."
  [schema]
  ; I don't know a good way to debias this in Hegel--it wants to pick the first
  ; option, or 0 in a range of integers, like 4x more than any other option.
  ; That makes it really hard to get to bugs with multiple inserts.
  (g/let [f (g/one-of (g/constant gen-select)
                      (g/constant gen-insert)
                      (g/constant gen-insert)
                      (g/constant gen-insert)
                      (g/constant gen-update)
                      (g/constant gen-update)
                      (g/constant gen-update)
                      (g/constant gen-delete))]
    (f schema)))

(defn gen-case
  "Generates a test case for a unique integer ID. A case is a map of:

  :id           A unique integer id
  :sql          A vector of SQL statements
  :teardown-sql A vector of SQL statements to tear down what this made."
  [id]
  (let [schema (gen-schema id)]
    {:id  id
     ; I don't quite know how to convince Hegel to generate long traces using
     ; new_collection--it almost always wants short histories of just a couple
     ; statements, and that doesn't find bugs well.
     :sql (g/collect [sqls (schema->create-sql schema)
                      {:min-size 1 :max-size 512}]
                      (conj sqls (gen-statement schema)))
     ;:sql (g/let [scale (apply g/one-of (mapv g/constant [1 10 100]))
     ;             n     (g/integer scale (* scale 10))]
     ;       (hc/with-span
     ;         (prn :scale scale :n n)
     ;         (into [] (take n (repeatedly (partial gen-statement schema))))
     ;         (->> #(gen-statement schema) repeatedly (take n) (into []))))
     :teardown-sql (schema->teardown-sql schema)}))

;; Debugging

(defn sample-cases
  "Generates a few random cases with Hegel. Helpful for debugging at the repl."
  [n]
  (hc/sample n (gen-case 0)))

(defn sample-exprs
  "Generates a few random expressions with Hegel. Helpful for debugging."
  [n]
  (hc/sample n
             (let [schema (gen-schema 0)
                   table  (gen-nth schema)]
               (gen-statement schema))))

;; Checker

(defn indented-pprint-str
  "Prints to a string, with the given indent string first."
  [indent x]
  (-> (str indent (with-out-str (pprint x)))
      (str/replace "\n" (str "\n" indent))
      (str/replace #"[\n\s]*$" "")))

(defn op-size
  "Roughly how 'big' is an operation? We use this to find small counterexamples
  to print out."
  [op]
  (->> op :value :sql (map (comp #(.size ^String %))) (reduce + 0)))

(defn write-case!
  "Writes a test case op to a file."
  [test
   {:keys [dir origin-dir?]}
   {:keys [value] :as op}]
  (let [{:keys [id origin sql]} value]
    (store/with-out-file
      test (str dir "/"
                (when origin-dir?
                  (str origin "/"))
                origin " " id ".txt")
      ; First, step-by-step with results.
      (doseq [{:keys [sql pg pdb] :as mop} sql]
        (print (gen.tc/mangle-table-str id sql))
        (println ";")
        (if (equiv-mop? mop)
          (println (indented-pprint-str "  " pdb))
          (do ; Oh, they're different
              (println "  ParticleDB:")
              (println (indented-pprint-str "    " pdb))
              (println "  Postgres:")
              (println (indented-pprint-str "    " pg))
              (println))))

      ; Then condensed case
      (println)
      (println "# Condensed case")
      (println)
      (doseq [sql (:teardown-sql (:value op))]
        (print (gen.tc/mangle-table-str nil sql))
        (println ";"))
      (println)
      (doseq [{:keys [sql]} (:sql (:value op))]
        (print (gen.tc/mangle-table-str nil sql))
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
                           (into []))
          smallest (->> interesting
                        (group-by (comp :origin :value))
                        (mapv (fn [[origin ops]]
                                (first (sort-by op-size ops)))))]
      (doseq [op final]
        (write-case! test {:origin-dir? false :dir "final"} op))
      (doseq [op smallest]
        (write-case! test {:origin-dir? false :dir "smallest"} op))
      (doseq [op interesting]
        (write-case! test {:origin-dir? true :dir "interesting"} op))

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
        ;hegel-worker (hegel-worker
        ;               {:test-cases 10000
        ;                :seed (rand/long)}
        ;               last-id
        ;               queue)]
        tc-worker (gen.tc/worker {:test-cases 10000
                                  :seed (rand/long)
                                  :max-statement-count
                                  (:compare-max-statement-count opts)}
                                 last-id
                                 queue)]
    {:client    (Client. nil nil nil)
     :generator (hegel-gen queue)
     :checker   (Checker.)}))
