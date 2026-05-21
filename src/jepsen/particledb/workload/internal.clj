(ns jepsen.particledb.workload.internal
  "A simple test for internal consistency. Each transaction either inserts or
  updates a key, then tries to read it."
  (:refer-clojure :exclude [read])
  (:require [clojure.tools.logging :refer [info warn]]
            [clojure [pprint :refer [pprint]]
                     [string :as str]]
            [dom-top.core :refer [loopr with-retry]]
            [elle.core :as elle]
            [jepsen [checker :as checker]
                    [client :as client]
                    [core :as jepsen]
                    [generator :as gen]
                    [history :as h]
                    [random :as rand]
                    [util :as util]]
            [jepsen.particledb [client :as c]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql.builder :as sqlb]
            [slingshot.slingshot :refer [try+ throw+]]
            [tesser.core :as t]))

(defn mop!
  "Applies a single transaction micro-operation. Returns micro-op, or nil if
  the operation did not take effect."
  [test conn [f k v]]
  (Thread/sleep (rand/zipf (:mop-delay test)))
  (case f
    :insert (c/try+ (info :insert k)
              (j/execute! conn [(str "INSERT INTO internal (id, val) "
                                     "VALUES (?, ?)")
                                k v])
              [f k v]
              (catch [:definite? true] e nil))

    :update (if (-> conn
                    (j/execute-one!
                      [(str "UPDATE internal SET val = ? WHERE id = ?")
                       v k])
                    :next.jdbc/update-count
                    pos?)
              [f k v])

    :r [f k (-> conn
                (j/execute-one! [(str "SELECT (val) FROM internal "
                                      "WHERE id = ?") k]
                                {:builder-fn rs/as-unqualified-lower-maps})
                :val)]))

(defrecord Client [node conn]
  client/Client
  (open! [this test node]
    (assoc this :conn (c/open test node)))

  (setup! [_ test]
    (j/execute! conn
                [(str "CREATE TABLE IF NOT EXISTS internal ("
                      "  id int not null primary key,"
                      "  val integer"
                      ")")]))

  (invoke! [_ test op]
    ;(j/with-transaction [t conn]
    (c/with-manual-txn test [conn conn]
      (assoc op
             :type :ok
             :value (->> (:value op)
                         (mapv (partial mop! test conn))
                         (remove nil?)
                         vec))))

  (teardown! [_ test])

  (close! [_ test]
    (c/close! conn)))

(defrecord Generator [next-id]
  gen/Generator
  (op [this test ctx]
    (loopr [next-id next-id
            txn     []]
           [i (range (inc (rand/zipf 10)))]
           (let [; A recent key. Moooostly these use the next id, but sometimes
                 ; it'll be three or five keys ago.
                 k (- next-id (rand/zipf (min next-id 5)))]
             (rand/weighted-branch
               ; Insert
               1 (recur (inc next-id)
                        (conj txn [:insert k (rand/long 100)]))

               ; Update
               4 (recur next-id (conj txn [:update k (rand/long 100)]))

               ; Read
               2 (recur next-id (conj txn [:r k nil]))))
           ; After we build the initial transaction, go through and read
           ; everything just to be sure.
           (let [txn (->> txn
                          (mapv second)
                          ; What the heck, maybe multiple reads will be fun
                          rand/shuffle
                          (mapv (fn [k] [:r k nil]))
                          (into txn))]
             ; Done building txn
             [(gen/fill-in-op {:f :txn, :value txn} ctx)
              (Generator. next-id)])))

  (update [this test ctx event]
    this))

(defn gen
  "Generates a stream of transactions which try to observe internal consistency
  anomalies."
  []
  (Generator. 0))

(defn internal-anomaly
  "Does an operation have an internal anomaly? If so, returns a map explaining
  it."
  [op]
  (when (h/ok? op)
    (loopr [state (transient {})]
           [[f k v :as mop] (:value op)]
           (case f
             :insert
             (recur (assoc! state k v))

             :update
             (recur (assoc! state k v))

             :r
             (if (contains? state k)
               (if (= v (state k))
                 (recur state)
                 ; Oops!
                 {:op op
                  :mop mop
                  :k k
                  :expected (get state k)
                  :actual v})
               ; New information
               (recur (assoc! state k v))))
           nil)))

(defrecord Checker []
  checker/Checker
  (check [this test history opts]
    (let [{:keys [n errors]}
          (->> (t/filter h/ok?)
               (t/filter (h/has-f? :txn))
               (t/fuse
                 {:n      (t/count)
                  :errors (->> (t/keep internal-anomaly)
                               (t/into []))})
               (h/tesser history))]
      {:valid? (not (seq errors))
       :errors errors
       :error-count (count errors)
       :txn-count n})))

(defn workload
  "A workload for an internal consistency test. Takes CLI opts."
  [opts]
  {:generator (gen)
   :client    (Client. nil nil)
   :checker   (Checker.)})
