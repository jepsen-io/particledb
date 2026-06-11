(ns jepsen.particledb.workload.repro
  "Supports writing reproductions of specific cases."
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
            [jepsen.sql [client :as sqlc]
                        [checker :as sql.checker]]
            [jepsen.particledb.pdb-client :as pdbc]
            [jepsen.postgres.client :as pgc]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [tesser.core :as t])
  (:import (clojure.lang Keyword
                         MapEntry
                         PersistentArrayMap
                         PersistentVector
                         PersistentQueue)
           (java.sql ResultSet
                     ResultSetMetaData
                     SQLException)
           (org.postgresql.util PSQLException)))

(defn mop!
  "Applies a single SQL statement, capturing errors as simple maps."
  [conn sql]
  (sqlc/try+
    (j/execute! conn sql)
    ; We want this to propagate up so the client can close/reconnect.
    (catch [:type :connection-closed] e
      (throw+ e))
    (catch (:type %) e
      e)
    (catch SQLException e
      {:type :sql-error :msg (.getMessage e)})
    (catch PSQLException e
      {:type :psql-error, :msg (.getMessage e)})
    (catch IllegalStateException e
      {:type :illegal-state-exception, :msg (.getMessage e)})))

; This is a generic client: it takes a vector of SQL strings and runs them
; against a JDBC client, returning a vector of their results as maps.
(defrecord Client []
  sqlc/Client
  (open! [this test conn node]
    this)

  (setup! [this test conn])

  (invoke! [this test conn op]
    (assoc op
           :type  :ok
           :value (mapv (fn mop [sql]
                          {:sql    sql
                           :result (mop! conn sql)})
                        (:value op))))

  (teardown! [this test conn])

  (close! [this test]))

(defrecord Checker [invalid-op?]
  checker/Checker
  (check [this test history opts]
    (let [res (->> (t/filter h/ok?)
                   (t/fuse
                     {:count  (t/count)
                      :errors (->> (t/filter invalid-op?)
                                   (t/into []))})
                   (h/tesser history))]
      (assoc res
             :valid? (not (seq (:errors res)))
             :error-count (count (:errors res))))))


(defn workload
  "Constructs a workload given CLI options. Special options:

      :protocol     :particledb or :postgres
      :generator    The generator of operations
      :invalid-op?  True if an ok op produced invalid results"
  [{:keys [protocol generator invalid-op?]
    :or {protocol :particledb}
    :as opts}]
  {:client (sqlc/client (Client.)
                        (merge opts
                               (case protocol
                                 :particledb
                                 {:jepsen.sql/open     pdbc/open
                                  :jepsen.sql/error-fn pdbc/error-fn}

                                 :postgres
                                 {:jepsen.sql/open     pgc/open
                                  :jepsen.sql/error-fn pgc/error-fn})))
   :generator generator
   :checker   (sql.checker/compose (Checker. invalid-op?) :repro)})
