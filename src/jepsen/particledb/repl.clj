(ns jepsen.particledb.repl
  "This is where the REPL starts."
  (:require [clojure [pprint :refer [pprint]]]
            [jepsen.particledb [pdb-client :as pdbc]]
            [jepsen [history :as h]
                    [store :as store]]
            [jepsen.postgres [client :as pgc]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql.builder :as sqlb]))

(defn open-pdb-pdb
  "Opens a particledb client to the given ParticleDB node."
  [node]
  (pdbc/open {} node))

(defn open-pdb-pg
  "Opens a postgres client to the given ParticleDB node."
  [node]
  (pdbc/open {:postgres-port 15432}, "n9"))

(defn open-pg
  "Opens a postgres client to the given Postgres node."
  [node]
  (pgc/open {:postgres-user "postgres"
             :postgres-password "pw"}
            "n9"))

(defn open2!
  "Opens and defs two clients: pgc, pdb-pdbc, and also a map `clients` for use
  with `splay`."
  [node]
  (def pgc      (open-pg node))
  (def pdb-pdbc (open-pdb-pdb node))
  (def clients  {:pg      pgc
                 :pdb-pdb pdb-pdbc}))

(defn open3!
  "Opens and defs three clients: pgc, pdb-pdbc, and pdb-pgc, and also a map
  `clients` for use with `splay`."
  [node]
  (def pgc      (open-pg node))
  (def pdb-pdbc (open-pdb-pdb node))
  (def pdb-pgc  (open-pdb-pg node))
  (def clients  {:pg      pgc
                 :pdb-pdb pdb-pdbc
                 :pdb-pg  pdb-pgc}))


(defn exec-fn
  "Takes a client and returns a function that takes [sql-str & args] vectors
  and calls j/execute!"
  [client]
  (partial j/execute! client))

(defn splay!
  "Takes a map of names to clients and an sql vector. Splays the queries out against
  all clients and returns a map of names to results from each client."
  ([sql]
   (splay! clients sql))
  ([clients sql]
   (into (sorted-map)
         (map (fn [[name client]]
                [name (j/execute! client sql)])
              clients))))

