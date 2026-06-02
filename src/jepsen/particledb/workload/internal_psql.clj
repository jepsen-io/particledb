(ns jepsen.particledb.workload.internal-psql
  "A ridiculous hack to make sure that this isn't some weird issue related to
  JDBC or the JVM Postgres client. We actually shell out to psql on the remote
  node and have it execute transactions as strings, parsing its output."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [string :as str]]
            [clojure.tools.logging :refer [info warn]]
            [dom-top.core :refer [loopr]]
            [jepsen [control :as c]
                    [checker :as checker]
                    [client :as client]
                    [random :as rand]
                    [store :as store]]
            [jepsen.control.core :as cc]
            [jepsen.sql.internal :as internal]))

(def slow-delay
  1)

(def newline-byte
  (aget (.getBytes "\n") 0))

(defn slow-transfer
  "Copies bytes from one array to another, breaking after newlines. Returns
  bytes copied."
  [in in-i out out-i len]
  ; No need to validate; our caller always computes safe bounds and this whole
  ; thing is a haaaaack
  (loop [in-i      in-i
         out-i     out-i
         remaining len
         copied    0]
    (if (= 0 remaining)
      ; Finished normally
      copied
      (let [x (aget in in-i)]
        (aset out out-i x)
        (if (= newline-byte x)
          (inc copied)
          (recur (inc in-i)
                 (inc out-i)
                 (dec remaining)
                 (inc copied)))))))

(defn slow-stream
  "Turns a string into a very slow input stream, to make transactions take some
  time."
  [test ^String s]
  (let [data (.getBytes s)
        i    (volatile! 0)]
    (proxy [java.io.InputStream] []
      (available []
        (- (alength data) @i))

      (close [])

      (read
        ; This path doesn't get called
        ([]
         (if (= @i (alength data))
           -1
           (do (Thread/sleep slow-delay)
               (let [r (aget data @i)]
                 (vswap! i inc)
                 r))))

        ; This is what actually gets used
        ([^bytes out]
         (if (= @i (alength data))
           -1
           (do (Thread/sleep (rand/zipf (:mop-delay test)))
               (let [n (- (alength data) @i)
                     n (slow-transfer data @i out 0 n)]
                 (vswap! i + n)
                 n))))

        ([^bytes out off len]
         (if (= @i (alength data))
           -1
           (do (Thread/sleep (rand/zipf (:mop-delay test))
               (let [n (min len
                            (- (alength data) @i)
                            (- (alength out) off))
                     n (slow-transfer data @i out off n)]
                 (vswap! i + n)
                 n)))))))))

(defn sql!
  "Runs an SQL string and returns results."
  [test sql]
  (let [stream (slow-stream test sql)
        res (-> {:cmd (->> ["psql" "-h" "localhost" "-p" (:postgres-port test)]
                           (map c/escape)
                           (str/join " "))
                 :in stream}
                c/wrap-cd
                c/wrap-sudo
                c/wrap-trace
                c/ssh*
                cc/throw-on-nonzero-exit
                c/just-stdout)]
    (assert (= 0 (.available stream)))
    res))

(defn mop->sql
  "Turns a micro-op into an SQL statement."
  [[f k v]]
  (case f
    :insert (str "INSERT INTO internal (id, val) VALUES (" k ", " v");")
    :update (str "UPDATE internal SET val = " v " WHERE id = " k ";")
    :r (str "SELECT val FROM internal WHERE id = " k ";")))

(defn txn->sql
  "Turns a transaction structure into an SQL string."
  [txn]
  (str "BEGIN;\n"
       (str/join "\n" (map mop->sql txn))
       "\nCOMMIT;\n"))

(defn parse
  "Takes a transaction structure at the CLI and the output of psql; parses it
  into a vector of [f k v] mops."
  [txn s]
  (let [err {:type :parse-error, :txn txn, :string s}]
  (loop [state            :begin
         [line :as lines] (str/split-lines s)
         txn              txn
         txn'             []]
    ;(prn :state state, :line line :txn txn :txn' txn')
    (when (= line "ROLLBACK")
      (throw+ {:type :rollback}))
    (case state
      :begin (if (= line "BEGIN")
               (recur :mop (next lines) txn txn')
               (throw+ (assoc err :point :begin)))
      ; Starting a micro-op
      :mop (let [[f k v :as mop] (first txn)]
             (if-not mop
               (recur :commit lines txn txn')
               (case f
                 :insert
                 (case line
                   "INSERT 0 1"
                   (recur :mop (next lines) (next txn) (conj txn' mop))

                   (throw+ (assoc err :point :unrecognized-insert)))

                 :update
                 (case line
                   ; Didn't happen
                   "UPDATE 0"
                   (recur :mop (next lines) (next txn) txn')

                   ; Happened
                   "UPDATE 1"
                   (recur :mop (next lines) (next txn) (conj txn' mop))

                   (throw+ (assoc err :point :unrecognized-update)))

                 :r (recur :r-header lines txn txn')

                 (throw+ (assoc err :point :unrecognized-mop)))))

      ; Reading the header
      :r-header (case (str/trim line)
                  ; No rows
                  "SELECT 0"
                  (recur :mop (next lines) (next txn) (conj txn' (first txn)))

                  ; Start of a table
                  "val"
                  (recur :r-divider (next lines) txn txn')

                  (throw+ (assoc err :point :r-header)))

      ; Reading the dividing line ---
      :r-divider (if (re-find #"^-+$" line)
                   (recur :r-val (next lines) txn txn')
                   (throw+ (assoc err :point :r-divider)))

      ; Reading the value
      :r-val (condp re-find line
               ; Empty table; read nothing
               #"\(0 rows\)"
               (recur :r-footer lines (next txn) (conj txn' (first txn)))

               ; A number! This is a value
               #"^ *\d+ *$"
               (let [[f k _] (first txn)
                     v       (parse-long (str/trim line))]
                 (recur :r-footer (next lines) (next txn)
                        (conj txn' [f k v])))
               (throw+ (assoc err :point :r-val)))

      ; And the footer (n rows), plus a blank line
      :r-footer (if (and (re-find #"^\(\d+ rows?\)$" line)
                         (or (nil? (second lines))
                             (re-find #"\s*" (second lines))))
                  (recur :mop (next (next lines)) txn txn')
                  (throw+ (assoc err :point :r-blank)))

      ; Commit
      :commit
      ; Sometimes, and I can't tell why, psql comes back with no COMMIT line
      ; even though we issued a COMMIT. But there's no error, and it exited
      ; normally. I'm not sure it's safe to expose this here, but if the system
      ; is *actually* SI, it would be extremely distressing for even a
      ; transaction which did not commit to observe internal inconsistency, so
      ; I'm going to let that pass through.
      (if (or (nil? line) (= "COMMIT" line))
        txn'
        (throw+ (assoc err :point :commit)))))))


(defrecord Client [node]
  client/Client
  (open! [this test node]
    (assoc this :node node))

  (setup! [this test]
    (c/with-node test node
      (sql! test
            (str "CREATE TABLE IF NOT EXISTS internal ("
                 "  id int not null primary key,"
                 "  val integer"
                 ");"))))

  (invoke! [this test op]
    (c/with-node test node
      (try+
        (let [txn (:value op)
              sql (txn->sql txn)
              res (sql! test sql)
              txn' (parse txn res)]
          (assoc op :type :ok, :value txn', :sql sql, :sql-out res))
        (catch [:type :rollback] e
          (assoc op :type :fail, :error :rollback))
        (catch [:type :parse-error] e
          (assoc op :type :info, :error e))
        (catch [:exit 1] e
          (condp re-find (:err e)
            #"could not serialize" (assoc op :type :fail, :error :could-not-serialize)

            #"duplicate key value" (assoc op :type :fail, :error :duplicate-key-value)
            (throw+ e))))))

  (teardown! [this test])

  (close! [this test]))

(defn checker
  "Like the internal checker, but we also write out files which show the input
  and output SQL nicely."
  [checker]
  (reify checker/Checker
    (check [this test history opts]
      (let [res (checker/check checker test history opts)]
        (when (seq (:errors res))
          (store/with-out-file test "internal-errors.sql"
            (doseq [{:keys [op mop k expected actual]} (:errors res)]
              (let [{:keys [value sql sql-out]} op]
                (pprint value)
                (println)
                (println sql)
                (println)
                (println sql-out)
                (println "-------------------------------")
                (println)))))
        res))))

(defn workload
  "Constructs a workload given CLI options."
  [opts]
  ; We work just like the internal workload, but we replace the client.
  (-> (internal/workload opts)
      (assoc :client (Client. nil))
      (update :checker checker)))
