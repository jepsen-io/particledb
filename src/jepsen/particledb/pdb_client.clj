(ns jepsen.particledb.pdb-client
  "This namespace provides workloads that use the official ParticleDB SDK
  client."
  (:require [clojure [pprint :refer [pprint]]]
            [clojure.tools.logging :refer [info warn]]
            [clj-commons.slingshot :refer [throw+]]
            [dom-top.core :refer [loopr]]
            [jepsen [sql :as sql]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs])
  (:import (clojure.lang ExceptionInfo)
           (java.net SocketException)
           (java.sql SQLException)
           (org.particledb Driver)))

(defn open
  "Opens a JDBC connection to the given node."
  [test node]
  (->> {:dbtype "particledb"
        ; First it was com.particledb, then ai.particledb, now
        ; org.particledb??? I feel like an LLM is probably responsible
        ; for this nonsense...
        :classname "org.particledb.Driver"
        :host node
        :port (:particledb-port test 5440)}
       j/get-datasource
       j/get-connection))

(defn error-fn
  "Turns ParticleDB exceptions into error maps."
  [e]
  (let [m (.getMessage e)]
    (condp identical? (class e)
      SocketException
      (condp re-find m
        "Connection reset"
        {:type :conn-reset}

        nil)

      SQLException
      (condp re-find m
        #"PDB JDBC: null"
        {:type :pdb-jdbc-null, :msg m, :definite? false}

        #"could not serialize access"
        {:type :could-not-serialize, :msg m, :definite? true}

        #"current transaction is aborted, commands ignored"
        {:type :aborted, :msg m, :definite? true}

        #"duplicate key value"
        {:type :duplicate, :msg m, :definite? true}
        nil)

      ExceptionInfo
      (let [data (ex-data e)]
        (cond (re-find #"Rollback failed handling" m)
              ; Ah, let's look at the root cause
              (recur (:handling data))

              true
              nil))
      nil)))

(def workloads
  "A map of workload names like :append-pdb to workload fns."
  (update-keys (sql/workloads {:open open, :error-fn error-fn})
               (fn [k]
                 (keyword (str (name k) "-pdb")))))
