(ns jepsen.particledb.db
  "Database setup and teardown."
  (:require [clojure [string :as str]]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [info warn]]
            [jepsen [control :as c :refer [|]]
                    [core :as jepsen]
                    [db :as db]
                    [generator :as gen]
                    [lazyfs :as lazyfs]
                    [util :as util :refer [await-fn meh]]]
            [jepsen.control [net :as cn]
                            [util :as cu]]
            [jepsen.os.debian :as debian]
            [jepsen.particledb.pdb-client :as pdbc]
            [jepsen.postgres.db :as pg]
            [clj-commons.slingshot :refer [try+ throw+]]))

(def dir
  "The top-level directory."
  "/opt/particledb")

(def bin-name
  "The name of the server binary."
  "particledb")

(def bin
  "Path to the server binary."
  (str dir "/" bin-name))

(def log-file
  "Where we direct logs from stdout/stderr"
  (str dir "/daemon.log"))

(def pid-file
  "Where we write the process' pidfile"
  (str dir "/particledb.pid"))

(def data-dir
  "ParticleDB data directory"
  (str dir "/data"))

(defn install!
  "Install ParticleDB"
  [test]
  (c/su
    ; ParticleDB logs ANSI color sequences to stdout/stderr, so we have to
    ; filter those out to get readable logs
    (debian/install ["ansifilter"])
    (c/exec :mkdir :-p dir)
    (let [version (:version test)]
      (if (.exists (io/file version))
        ; Upload local binary
        (do (info "Uploading local version" version)
            (c/upload version bin))
        (let [url (str "https://pdb-releases-193278869920-us-east-1.s3.us-east-1.amazonaws.com/v" (:version test) "/particledb-linux-x86_64")
              path (cu/cached-wget! url)]
          (c/exec :cp path bin)))
      (c/exec :chmod :+x bin))))

(defn wipe!
  "Kills and wipes data on a node."
  [test node]
  (db/kill! (:db test) test node)
  (c/su (c/exec :rm :-rf data-dir log-file pid-file))
  :wiped)

(defrecord DB [lazyfs]
  db/DB
  (setup! [this test node]
    (install! test)
    (when (:lazyfs test)
      (db/setup! lazyfs test node))

    (db/start! this test node)
    (cu/await-tcp-port (:postgres-port test)
                       {:log-interval 10000}))

  (teardown! [this test node]
    ; Always tear down, in case we have a previous, crashed state left over
    (db/teardown! lazyfs test node)
    (wipe! test node))

  db/LogFiles
  (log-files [this test node]
    ; The log file will have ANSI color escapes, so we rewrite it
    (c/su
      (let [plain-file (str log-file ".plain")]
      (c/exec :ansifilter :-p :-i log-file :-o plain-file)
      {plain-file               "daemon.log"
       (cu/tarball! data-dir)   "data.tar.gz"})))

  db/Primary
  (primaries [_ test])
  (setup-primary! [_ test node])

  db/Pause
  (pause! [_ test node]
    (c/su (cu/grepkill! :stop bin-name))
    :paused)

  (resume! [_ test node]
    (c/su (cu/grepkill! :cont bin-name))
    :resumed)

  db/Kill
  (kill! [_ test node]
    (info "Killing ParticleDB")
    (c/su (cu/grepkill! :kill bin-name))
    (when (:lazyfs test)
      (lazyfs/lose-unfsynced-writes! lazyfs))
    :killed)

  (start! [_ test node]
    (c/su
      (cu/start-daemon!
        {:logfile log-file
         :pidfile pid-file
         :chdir   dir}
         bin
         :start
         :--data-dir data-dir
         :--pg-addr (str "0.0.0.0:" (:postgres-port test))
         ; These don't seem to work
         ;:--auth-method "password"
         ;:--pg-password (:postgres-password test)
         ; So let's try turning off auth entirely
         :--no-auth
         (when-let [m (:wal-sync-mode test)] [:--wal-sync-mode (name m)])
         (when-let [m (:txn-mode test)]      [:--txn-mode      (name m)])
         ; This makes it deeply frustrating to debug
         :--connection-timeout 0
         :--max-connections 16384))))

(defn db
  "Constructs a new DB for the test, given CLI opts."
  [opts]
  (map->DB {:lazyfs (lazyfs/db {:dir data-dir})}))

(defrecord JointDB [pg pdb]
  db/DB
  (setup! [this test node]
    (let [pg  (future (db/setup! pg test node))
          pdb (future (db/setup! pdb test node))]
      @pg
      @pdb))

  (teardown! [this test node]
    (db/teardown! pg test node)
    (db/teardown! pdb test node))

  db/LogFiles
  (log-files [this test node]
    (merge (db/log-files pg test node)
           (db/log-files pdb test node)))

  db/Pause
  (pause! [_ test node]
    (db/pause! pg test node)
    (db/pause! pdb test node))

  (resume! [_ test node]
    (db/resume! pg test node)
    (db/resume! pdb test node))

  db/Kill
  (kill! [_ test node]
    (db/kill! pg test node)
    (db/kill! pdb test node))

  (start! [_ test node]
    (db/start! pg test node)
    (db/start! pdb test node)))

(defn joint-db
  "Postgres and ParticleDB together, for select workloads."
  [opts]
  (if (#{:compare} (:workload opts))
    (JointDB. (pg/db {})
              (db opts))))
