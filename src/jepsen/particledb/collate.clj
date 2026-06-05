(ns jepsen.particledb.collate
  "A very small test, just at the REPL, to figure out what collation ParticleDB
  actually uses."
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
            [next.jdbc.result-set :as rs]))

(defn check [node n]
  (let [pg (pgc/open {:postgres-user "postgres"
                       :postgres-password "pw"}
                      node)
        pdb (pgc/open {:postgres-port 15432} node)
        ; Ahaha Hegel strings are WAY too weird for the pg client
        strings (distinct
                  (hc/sample
                    n
                    (hc/gen
                      (g/regex "(\\d|\\s|\\w){1,3}"
                               {:full-match? true}))))
        collations ["unicode"
                    "ucs_basic"
                    ;"pg_unicode_fast"
                    "pg_c_utf8"
                    "C"
                    ]
        schema (str "CREATE TABLE coll ("
                    (str/join ", "
                              (map (fn [collation]
                                     (str "t_" collation " text COLLATE \""
                                          collation "\""))
                                   collations))
                    ")")]
    ; Create tables
    (j/execute! pg ["DROP TABLE IF EXISTS coll"])
    (j/execute! pg [schema])
    (j/execute! pdb ["DROP TABLE IF EXISTS coll"])
    (j/execute! pdb ["CREATE TABLE coll (t text)"])
    ; Insert a bunch of strings
    (doseq [string strings]
      (j/execute! pg (into [(str "INSERT INTO coll VALUES ("
                                 (str/join "," (repeat (count collations) "?"))
                                 ")")]
                           (repeat (count collations) string)))
      (j/execute! pdb [(str "INSERT INTO coll VALUES (?)")
                           string]))
    ; Figure out what Postgres thinks they should sort as
    (let [extract (partial mapv (comp val first))
          orders (->> collations
                      (mapv (fn [collation]
                             [collation
                              (extract (j/execute! pg [(str "SELECT t_" collation " FROM coll ORDER BY t_" collation " ASC")]))]))
                      (into {}))
          ; And what order PDB uses
          pdb-order (extract (j/execute! pdb ["SELECT t FROM coll ORDER BY t ASC"]))]
      (doseq [s strings] (prn s))
      (println)
      (doseq [collation collations]
        (if (= (get orders collation) pdb-order)
          (println collation "\tmatches!")
          (println collation "\tdoes not match!"))))))
