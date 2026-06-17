(ns jepsen.particledb.cli
  "Command-line entry point for ParticleDB tests"
  (:gen-class)
  (:require [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen ;[antithesis :as a]
                    [checker :as checker]
                    [cli :as cli]
                    [control :as control]
                    [generator :as gen]
                    [nemesis :as jepsen.nemesis]
                    [os :as os]
                    [random :as rand]
                    [sql :as sql]
                    [tests :as tests]
                    [util :as util :refer [default]]]
            [jepsen.particledb [db :as db]
                               [nemesis :as nemesis]
                               [pdb-client :as pdb-client]
                               [repl :as repl]]
            [jepsen.particledb.workload
             [compare :as compare]
             [internal-psql :as internal-psql]
             [repro-character-dups :as repro-character-dups]
             [repro-missing-rows :as repro-missing-rows]]
            [jepsen.postgres [cli :as pcli]]
            [jepsen.os.debian :as debian]
            [jepsen.sql [encoding :as encoding]]))

(def wal-sync-modes
  "The set of WAL sync modes supported by ParticleDB"
  #{:sync :groupsync :nosync})

(def txn-modes
  "The set of txn modes supported by ParticleDB"
  #{:fast :occ-v3 :zero-copy-occ, :serializable-ssi, :table-2pl})

(defn expected-consistency-model
  "What consistency level do we expect for a given transaction mode and wal
  sync mode?"
  ; I *think* these are the defaults, but the docs contradict themselves so ???
  [{:keys [txn-mode wal-sync-mode]
    :or {txn-mode :occ-v3, wal-sync-mode :groupsync}}]
  (cond ; In fast txn mode, they start listing allowed anomalies at P4 and A5A,
        ; and also say that "no dirty reads", so I think we should be at...
        ; read committed? Maybe MAV? SI is out, not sure about causal.
        (= txn-mode :fast)
        :monotonic-atomic-view

        ; Having ruled out `fast`, we're supposed to be at *least* SI. But if
        ; we're running without sync, then we should expect some transactions
        ; to be lost on power failure. But! We should not, I think, expect G0,
        ; G1a, G1b, or G1c, and we shouldn't observe internal anomalies. I'm
        ; guessing that might put us at... read atomic?
        (= wal-sync-mode :nosync)
        :read-atomic

        ; OK, we're running in one of the two safe sync modes
        true
        (case txn-mode
          ; They explicitly say SI, but I'm willing to bet it's actually strong
          ; SI.
          :occ-v3         :strong-snapshot-isolation
          ; Ditto
          :zero-copy-occ  :strong-snapshot-isolation
          ; The internal docs say "not classified", which is silly;
          ; serializable is *right there* in the name. In PG this is actually
          ; strong-1SR, so let's try that here; if we do find realtime/process
          ; anomalies that's at least worth discussing.
          :serializable-ssi :strong-serializable
          ; The internal docs explicitly claim "Strict Serializable".
          :table-2pl        :strong-serializable)))

(def workloads
  "A map of workload names to functions that take CLI options and return
  workload maps"
  (merge ; These workloads are directly from Postgres
         (update-keys pcli/workloads
                      (fn [k]
                        (keyword (str (name k) "-pg"))))
         ; And these use the ParticleDB SDK
         pdb-client/workloads
         {:none                 (constantly tests/noop-test)
          :compare              compare/workload
          :internal-psql        internal-psql/workload
          :repro-character-dups repro-character-dups/workload
          :repro-missing-rows   repro-missing-rows/workload}))

(def standard-workloads
  "All the workloads we run by default."
  [:append-pg
   :append-pdb
   :compare
   :internal-pg
   :internal-pdb
   :internal-psql
   :rw-pg
   :rw-pdb])

(def nemeses
  "Basic nemeses we have available."
  #{:kill
    :pause
    :partition
    :packet
    :clock
    :bitflip-file-chunks
    :snapshot-file-chunks
    :truncate-file})

(def file-corruption-nemeses
  "Which nemeses corrupt files?"
  #{:bitflip-file-chunks
    :snapshot-file-chunks
    :truncate-file})

(def db-node-targets
  "Different ways we can target single nodes for database faults."
  #{:one
    :minority
    :majority
    :all})

(def standard-nemeses
  "Combinations of nemeses we run by default."
  [; Nothing
   []
   ; One fault at a time
   [:partition]
   [:kill]
   [:pause]
   [:clock]
   [:packet]
   [:bitflip-file-chunks  :kill]
   [:snapshot-file-chunks :kill]
   [:truncate-file        :kill]
   ; General chaos
   [:partition :pause :kill :clock :bitflip-file-chunks
    :snapshot-file-chunks :truncate-file-chunks]
   ])

(def special-nemeses
  "A map of special nemesis names to collections of faults."
  {:none []
   :all (peek standard-nemeses)})


(defn parse-comma-kws
  "Takes a comma-separated string and returns a collection of keywords."
  [spec]
  (->> (str/split spec #",")
       (remove #{""})
       (mapv keyword)))

(defn parse-nemesis-spec
  "Takes a comma-separated nemesis string and returns a collection of keyword
  faults."
  [spec]
  (->> (parse-comma-kws spec)
       (mapcat #(get special-nemeses % [%]))
       set))

(defn test-name
  "Takes CLI options and constructs a test name as a string."
  [opts]
  (str (str/replace (:version opts) "/" "-") " "
       (name (:workload opts))
       (when-let [m (:wal-sync-mode opts)]
         (str " wal=" (name m)))
       (when-let [m (:txn-mode opts)]
         (str " txn=" (name m)))
       (when (:lazyfs opts)
         " lazyfs")
       (when-let [cft (:corrupt-file-type opts)]
         (str " cft=" (name cft)))
       (let [n (:nemesis opts)]
         (when (seq n)
           (str " " (->> n (map name) sort (str/join ",")))))))

(defn exceptions-checker
  "Normally we let unexpected exceptions pass, but ParticleDB has found *so
  many* incredibly weird ways to fail--like totally forgetting a column
  exists--that I think it's safest for us to flag them."
  []
  (let [checker (checker/unhandled-exceptions)]
    (reify checker/Checker
      (check [this test history opts]
        (let [r (checker/check checker test history opts)]
          (assoc r :valid? (not (seq (:exceptions r)))))))))

(defn particledb-test
  "Takes CLI options and constructs a Jepsen test map"
  [opts]
  (let [seed (rand/long)
        _ (rand/set-seed! seed)
        opts            (default opts :expected-consistency-model
                          (expected-consistency-model opts))
        workload-name   (:workload opts)
        workload        ((workloads workload-name) opts)
        db              (db/joint-db opts)
        os              debian/os
        nemesis         (nemesis/package
                          {:db            db
                           :nodes         (:nodes opts)
                           :stable-period (:nemesis-stable-period opts)
                           :interval      (:nemesis-interval opts)
                           :faults        (:nemesis opts)
                           :partition     {:targets [:one :majority]}
                           :pause         {:targets (:db-node-targets opts)}
                           :kill          {:targets (:db-node-targets opts)}
                           :packet        {:targets [:one :minority :all]
                                           :behaviors
                                           [{:delay {:time   "100ms"
                                                     :jitter "50ms"}}]
                                           }})
        workload-gen (->> (:generator workload)
                          (gen/stagger (/ (:rate opts)))
                          gen/clients)
        nemesis-gen (->> (:generator nemesis)
                         gen/nemesis)
        ; Main phase generator
        gen (->> workload-gen
                   (gen/nemesis
                     (gen/phases
                       (gen/sleep (:initial-quiet-period opts))
                       (:generator nemesis))))
        ; Limit
        ;gen (if (a/antithesis?)
        ;      (a/early-termination-generator
        ;        {:interval (:antithesis-interval opts)} gen)
        ;      (gen/time-limit (:time-limit opts) gen))
        gen (gen/time-limit (:time-limit opts) gen)
        ; And afterwards, recover and final gen
        gen (gen/phases
              gen
              ; Recover
              (gen/nemesis
                [(gen/log "Recovering")
                 (:final-generator nemesis)
                 (gen/log "Beginning final generator")])
              ; Final gen
              (->> (:final-generator workload)
                   (gen/time-limit (:final-time-limit opts))
                   gen/clients))
        ; And wrap
        gen (if-let [wrap (:wrap-generator workload)]
              (wrap gen)
              gen)
        ; The compare test bypasses all that; no faults, we just let hegel
        ; drive.
        gen (if (#{:compare} workload-name)
              workload-gen
              gen)]
    (-> tests/noop-test
        (merge
          opts
          {:name     (test-name opts)
           :seed     seed
           :os       os
           :db       db
           :checker  (checker/flatten
                       (checker/compose
                         {:perf       (checker/perf)
                          :clock      (checker/clock-plot)
                          :stats      (checker/stats)
                          :exceptions (exceptions-checker)
                          :workload   (:checker workload)})
                       :workload)
           ;:client    (a/client (:client workload))
           :client    (:client workload)
           ;:nemesis   (if (a/antithesis?)
           ;             jepsen.nemesis/noop
           ;             (:nemesis nemesis jepsen.nemesis/noop))
           :nemesis   (:nemesis nemesis jepsen.nemesis/noop)
           :plot      {:nemeses (:perf nemesis)}
           :generator gen
           ;:logging {:overrides
           ;          {"..." "FATAL"}}
           })
        ;a/test
        )))

(def cli-opts
  "Command-line option specification"
  (cli/merge-opt-specs
    sql/cli-opts
    [[nil "--antithesis" "Forces Antithesis mode. Useful for debugging in local docker."]

     [nil "--concurrency NUMBER" "How many workers should we run? Must be an integer, optionally followed by n (e.g. 3n) to multiply by the number of nodes."
      :default  "3n"
      :validate [(partial re-find #"^\d+n?$")
                 "Must be an integer, optionally followed by n."]]

     [nil "--db-node-targets TARGETS" "A comma-separated list of ways to target DB nodes for faults, like 'one,majority'"
      :default  [:one :majority :all]
      :parse-fn parse-comma-kws
      :validate [(partial every? db-node-targets) (cli/one-of db-node-targets)]]

     [nil "--compare-origin PATTERN" "Takes a pattern matching origin strings in the compare workload, like 'different (data|columns)'. Hunts for that kind of bug and ignores all others, except those of unknown origin."
      :parse-fn re-pattern
      :default #""]

     [nil "--compare-max-statement-count COUNT" "The maximum number of DML statements generated in a single compare history."
      :default  64
      :parse-fn parse-long
      :validate [pos? "Must be positive"]]

     [nil "--corrupt-file-type TYPE" "What kind of files to corrupt. See nemesis/file-types."
      :parse-fn keyword
      :validate [nemesis/file-types (cli/one-of nemesis/file-types)]]

     ; Not sure whether we should do this or not; ParticleDB could just
     ; implement support for decimals.
     #_(let [encodings (-> (mapv :name encoding/all-encodings)
                         set
                         (disj :decimal))]
       [nil "--encodings ENCODING_NAMES" "The various ways we can encode values in SQL columns. I don't think ParticleDB can handle decimals, so we'll skip that one."
        :default (sort encodings)
        :parse-fn parse-comma-kws
        :validate [(partial every? encodings) (cli/one-of encodings)]])

     [nil "--expected-consistency-model MODEL" "What level of isolation do we *expect* to observe? Defaults are chosen based on wal-sync-mode and txn-mode."
      :parse-fn keyword]

     [nil "--final-time-limit SECONDS" "How long should we run the final generator for, at most? In seconds."
      :default  300
      :parse-fn read-string
      :validate [#(and (number? %) (pos? %)) "must be a positive number"]]

     [nil "--initial-quiet-period SECONDS" "How long to wait before beginning faults"
      :default  10
      :parse-fn read-string
      :validate [#(and (number? %) (pos? %)) "must be a positive number"]]

     [nil "--[no-]lazyfs" "Mounts data dir in a lazy filesystem that can lose writes on kill."]

     [nil "--nemesis FAULTS" "A comma-separated list of nemesis faults to enable"
      :default  #{}
      :parse-fn parse-nemesis-spec
      :validate [(partial every? (into nemeses (keys special-nemeses)))
                 (str (cli/one-of nemeses) " or the special nemeses, which " (cli/one-of special-nemeses))]]

     [nil "--nemesis-interval SECS" "Roughly how long between nemesis operations."
      :default  5
      :parse-fn read-string
      :validate [pos? "Must be a positive number."]]

     [nil "--nemesis-stable-period SECS" "If given, rotates the mixture of nemesis faults over time with roughly this period."
      :default  30
      :parse-fn parse-long
      :validate [pos? "Must be a positive number."]]

     [nil "--particledb-port NUMBER" "What port should we connect to when talking to the ParticleDB interface?"
      :default 5440
      :parse-fn parse-long]

     [nil "--postgres-password PASS" "What password should we use to connect to postgres?"
      :default "jepsenpw"]

     [nil "--postgres-sslmode MODE" "What sslmode should we use to connect to the postgres interface? Should be either require or disable."
      :default "disable"
      :parse-fn str
      :validate [#{"require" "disable"}
                 "Should be one of require, or disable"]]

     [nil "--postgres-port NUMBER" "What port should we connect to when talking to the postgres interface?"
      :default 15432
      :parse-fn parse-long]

     [nil "--postgres-user NAME" "What username should we use to connect to the Postgres interface?"
      ; While you can set a password, there doesn't seem to be any option to
      ; configure the username! Some docs say particledb, others say admin.
      ; Neither seems to work! We bypass this for now by passing `--no-auth`, but
      ; I'm leaving it here in case it works later.
      :default "particledb"]

     [nil "--prepare-threshold INT" "Passes a prepareThreshold option to the JDBC spec. Prepared statements cause the Postgres JDBC driver to explode with NoSuchElementException at java.util.ArrayDeque/removeFirst (ArrayDeque.java:361). It's fine in Postgres; this seems to be a ParticleDB-specific thing. We set this *very* high to work around that."
      :parse-fn parse-long
      :default Integer/MAX_VALUE]

     ["-r" "--rate HZ" "Approximate request rate, in hz"
      :default  10000
      :parse-fn read-string
      :validate [pos? "Must be a positive number."]]

     ; Right now ParticleDB doesn't support savepoints.
     [nil "--[no-]savepoints" "Does this database support savepoints?"
      :default false]

     [nil "--seed INTEGER" "A seed for the random number generator. Useful for reproducing `compare` runs."
     :parse-fn parse-long]

     [nil "--time-limit SECONDS"
      "Excluding setup and teardown, how long should a test run for, in seconds?"
      :default  300
      :parse-fn #(Long/parseLong %)
      :validate [pos? "Must be positive"]]

     [nil "--txn-mode MODE" "The ParticleDB transaction mode."
      :parse-fn keyword
      :validate [txn-modes (cli/one-of txn-modes)]]

     ["-v" "--version VERSION" "What version should we test? Can either be a version string like '1.1.4' or a path to a local binary on the control node."
      :default "1.1.4"]

     [nil "--wal-sync-mode MODE" "The WAL sync mode for ParticleDB"
      :parse-fn keyword
      :validate [wal-sync-modes (cli/one-of wal-sync-modes)]]

     ["-w" "--workload NAME" "What workload should we run?"
      :default  :append-pdb
      :parse-fn keyword
      :validate [workloads (cli/one-of workloads)]]
     ]))

(defn all-tests
  "Turns CLI options into a sequence of tests."
  [opts]
  (let [nemeses   (if-let [n (:nemesis opts)]  [n] standard-nemeses)
        workloads (if-let [w (:workload opts)] [w] standard-workloads)
        lazyfs    (let [l (:lazyfs opts)]
                    (if (nil? l)
                      [false true]
                      [l]))
        txn-modes (if-let [t (:txn-mode opts)]
                    [t]
                    (->> txn-modes
                         ; We sort of expect fast to be a tire fire; I'm not
                         ; even sure how to go about testing it.
                         (remove #{:fast})
                         (sort)))
        wal-modes (if-let [w (:wal-sync-mode opts)] [w] (sort wal-sync-modes))
        cfts        (let [cft (:corrupt-file-type opts)]
                      (if (nil? cft)
                        (keys nemesis/file-types)
                        [cft]))]
    (for [i     (range (:test-count opts))
          l     lazyfs
          n     nemeses
          ft    (if (seq (set/intersection file-corruption-nemeses (set n)))
                  cfts
                  [nil])
          t     txn-modes
          wm    wal-modes
          w     workloads]
      (particledb-test (-> opts
                           (assoc :lazyfs             l
                                  :nemesis            n
                                  :corrupt-file-type  ft
                                  :txn-mode           t
                                  :wal-sync-mode      wm
                                  :workload           w))))))
(defn opt-fn!
  "Post-processes CLI options. As a side effect, we initialize the RNG here."
  [{:keys [options] :as parsed}]
  (when-let [seed (:seed options)]
    (rand/set-seed! seed))
  parsed)

(def repro-watch-cmd
  "Watch an SQL file and try to play it against both PG and
  PDB. A VERY rough hack."
  {"repro-watch"
   {:opt-spec []
    :opt-fn identity
    :usage ""
    :run (fn repro-watch-cmd [{:keys [options arguments]}]
           (let [[node file] arguments
                 file (or file "repro.sql")]
             ; hack hack haaaaaack
             (repl/repro-watch! node file)
             (info "Watching" file "for changes")
             (while true
               (Thread/sleep 1000))))}})

(defn -main
  "Handles command line arguments. Can either run a test, or a web server for
  browsing results."
  [& args]
  ;(a/with-rng
    (cli/run! (merge (cli/single-test-cmd {:test-fn  particledb-test
                                           :opt-fn   opt-fn!
                                           :opt-spec cli-opts})
                     (cli/test-all-cmd
                       {:tests-fn all-tests
                        :opt-fn   opt-fn!
                        :opt-spec (cli/without-defaults-for
                                    [:workload :nemesis :lazyfs]
                                    cli-opts)})
                     (cli/serve-cmd)
                     repro-watch-cmd)
              args))
