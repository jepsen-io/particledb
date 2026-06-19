(ns jepsen.particledb.workload.compare
  "For this workload, we stand up a Postgres and a ParticleDB, generate random
  queries, and compare their results."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [clojure.tools.logging :refer [info warn]]
            [dom-top.core :refer [loopr]]
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

                   ; You can select nonexistent columns!
                   #"column .+ does not exist"
                   "column does not exist"

                   #"operator is not unique"
                   "operator not unique"

                   ; And put giant floats into tiny integer fields
                   #"out of range"
                   "out of range"

                   #"overflow"
                   "overflow"

                   "unknown postgres error"

                   ; ParticleDB doesn't do NOT NULL
                   #"violates not-null constraint"
                   "not-null constraint"

                   ; Or duplicate constraints
                   #"violates unique constraint"
                   "unique constraint")

                 ; PDB threw an error
                 (and (map? pdb) (:msg pdb))
                 (condp re-find (:msg pdb)
                   ; I guess you can't store infinity???
                   #"Bad value for type double"
                   "bad value for type double"

                   #"complex result"
                   "complex result"

                   #"could not serialize access"
                   "could not serialize"

                   #"division by zero"
                   "division by zero"

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

                   #"out of range"
                   "out of range"

                   #"overflow"
                   "overflow"

                   #"type mismatch"
                   "type mismatch"

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

(defrecord QueueGen [; A mutable queue of test cases someone wants us to try.
                     ; An atom to a PersistentQueue.
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

        ; When we complete an sql event, we'll send its result back to the
        ; promise
        :ok
        (let [id     (:id (:value event))
              result (get results id)
              _ (assert result)
              status (:status (:value event))
              _ (assert (keyword? status))]
          ; Send back
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

(defn queue-gen
  "A generator that draws operations from a queue. Takes an
  atom with a PersistentQueue of test cases."
  [queue]
  (QueueGen. queue {}))

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
        (if (= pdb pg)
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

(defn workload
  "Constructs a workload given CLI options."
  [opts]
  (let [last-id (atom -1)
        queue   (atom PersistentQueue/EMPTY)
        tc-worker (gen.tc/worker {:test-cases 10000
                                  :concurrency (:concurrency opts)
                                  :seed (rand/long)
                                  :max-statement-count
                                  (:compare-max-statement-count opts)}
                                 last-id
                                 queue)]
    {:client    (Client. nil nil nil)
     :generator (queue-gen queue)
     :checker   (Checker.)}))
