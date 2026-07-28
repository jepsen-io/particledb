(ns jepsen.particledb.repl
  "This is where the REPL starts."
  (:require [clojure [pprint :refer [pprint]]
                     [string :as str]]
            [clojure.java.io :as io]
            [clojure.term.colors :as colors]
            [clojure.tools.logging :refer [info warn]]
            [dom-top.core :refer [loopr]]
            [jepsen.particledb [pdb-client :as pdbc]]
            [jepsen.particledb.workload.repro :as repro]
            [jepsen [history :as h]
                    [store :as store]]
            [jepsen.postgres [client :as pgc]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql.builder :as sqlb]
            [juxt.dirwatch :refer [watch-dir]]))

(defn open-pdb-pdb
  "Opens a particledb client to the given ParticleDB node."
  [node]
  (pdbc/open {} node))

(defn open-pdb-pg
  "Opens a postgres client to the given ParticleDB node."
  [node]
  (pgc/open {:postgres-port 15432}, "n9"))

(defn open-pg
  "Opens a postgres client to the given Postgres node."
  [node]
  (pgc/open {:postgres-user "postgres"
             :postgres-password "pw"
             :postgres-port 5432}
            "n9"))

(defn open2!
  "Opens and defs two clients: pgc, pdb-pdbc, and also a map `clients` for use
  with `splay`."
  [node]
  (def pgc      (open-pg node))
  (def pdb-pdbc (open-pdb-pdb node))
  (def pdb-pgc  nil)
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

(defmacro with-unified-sql-client-errors
  "Evaluates body with a combined error handling fn for both pg and pdb"
  [& body]
  `(binding [jepsen.sql.client/*error-fn*
             (fn ~'error-fn [err#]
               (or (pdbc/error-fn err#)
                   (pgc/error-fn err#)))]
     ~@body))

(defn splay!
  "Takes a map of names to clients and an sql vector. Splays the queries out
  against all clients and returns a map of names to results from each client."
  ([sql]
   (splay! clients sql))
  ([clients sql]
   (with-unified-sql-client-errors
     (into (sorted-map)
           (map (fn [[name client]]
                  [name (repro/mop! client sql)])
                clients)))))

(defn repro-text!
  "Opens all three SQL clients. Takes a string with SQL statements, one per
  line. Executes those statements sqeuentially against each client, and prints
  out the results."
  [node text]
  (open3! node)
  (with-unified-sql-client-errors
    (let [statements (->> (str/split-lines text)
                          (remove str/blank?)
                          (mapv vector))
          ; Play forward all statements on each client, one client at a time
          results (->> clients
                       (mapv (fn [[client-name client]]
                               [client-name
                                (mapv (partial repro/mop! client) statements)]))
                       (into {}))
          clients (sort (keys results))
          max-db-width (reduce max 0 (map (comp count str) clients))]
      (loopr [i 0]
             [[sql] statements]
             (let [results (->> clients
                                (map (fn [client]
                                       [client (get-in results [client i])]))
                                (into {}))
                   color (if (or ; Errors
                                 (every? map? (vals results))
                                 ; Regular results
                                 (apply = (vals results)))
                           colors/white
                           colors/red)]
               (do (println sql)
                   (doseq [[client result] results]
                     (println
                       (color
                         (format (str "  %" max-db-width "s %s")
                                 client (pr-str result)))))
                   (recur (inc i))))))
    nil))

(defn repro-file!
  "Takes a filename with SQL statements, one per line, and executes them against all bound clients, printing results. Default file is repro.sql."
  ([node]
   (repro-file! node "repro.sql"))
  ([node file]
   (repro-text! node (slurp file))))

(defn repro-watch!
  "Like repro-file!, but watches the file for changes."
  ([node]
   (repro-watch! node "repro.sql"))
  ([node target-file]
   (repro-file! node target-file)
   (let [last-time (atom (System/nanoTime))]
     (watch-dir
       (fn [{:keys [file action] :as event}]
         (when (and (#{:create :modify} action)
                    (= target-file (.getName file)))
           (let [[t t'] (reset-vals! last-time (System/nanoTime))]
             (when (< 5e8 (- t' t))
               ; Ugh, another hack: it takes a few millis to write a file
               (Thread/sleep 5)
               (repro-file! node file)
               (prn)))))
       (io/file ".")))))
