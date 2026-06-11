(ns jepsen.particledb.workload.repro-character-dups
  "This is a bug in ParticleDB where updating a character(n) field which was
  written with fewer than n characters would produce two copies of it."
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
            [jepsen.particledb.workload.repro :as repro]
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
                     ResultSetMetaData)
           (org.postgresql.util PSQLException)))

(defn gen-case
  "Makes a vector of SQL statements for case i."
  [i]
  (let [table (str "t_" i)]
    [[(str "CREATE TABLE " table " (a character(3) PRIMARY KEY)")]
     [(str "INSERT INTO " table " (a) VALUES ('um')")]
     [(str "UPDATE " table " SET a = 'no'")]
     [(str "SELECT * FROM " table)]
     [(str "UPDATE " table " SET a = '!'")]
     [(str "SELECT * FROM " table)]
     [(str "UPDATE " table " SET a = 'cat'")]
     [(str "SELECT * FROM " table)]
     [(str "INSERT INTO " table " (a) VALUES ('why')")]
     [(str "SELECT * FROM " table)]
     ]))

(defn gen
  "Let's try running the same pattern over and over again"
  []
  (->> (range)
       (map (fn [i]
                {:f :sql
                 :value (gen-case i)}))))

(defn invalid?
  "Is an operation invalid? Did it return anything other than a single row?"
  [{:keys [value] :as op}]
  (not= #{{:a "cat"}
          {:a "why"}}
        (set (:result (last value)))))

(defn workload
  "Constructs a workload given CLI options."
  [opts]
  (repro/workload
    {:generator   (gen)
     :invalid-op? invalid?}))
