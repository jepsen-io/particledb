(ns jepsen.particledb.nemesis
  "Fault injection."
  (:require [clojure [pprint :refer [pprint]]
                     [set :as set]]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [info warn]]
            [dom-top.core :refer [real-pmap loopr]]
            [jepsen [control :as c]
                    [db :as jdb]
                    [nemesis :as n]
                    [generator :as gen]
                    [net :as net]
                    [util :as util]
                    [random :as rand]
                    [role :as role]]
            [clojure.set :as set]
            [jepsen.control.util :as cu]
            [jepsen.nemesis [combined :as nc]
                            [membership :as m]
                            [file :as nf]
                            [time :as nt]]
            [jepsen.particledb [db :as db]]
            [slingshot.slingshot :refer [try+ throw+]]))

(defn package-gen-helper
  "Helper for package-gen. Takes a collection of packages and draws a random
  nonempty subset of them."
  [packages]
  (when (seq packages)
    (let [pkgs (->> packages
                    ; And pick a random subset of those
                    util/random-nonempty-subset
                    vec)]
      ; If we drew nothing, try again.
      (if (seq pkgs)
        pkgs
        (do ; (info "no draw, retrying")
            (recur packages))))))

(defn package-gen
  "For long-running tests, it's nice to be able to have periods of no faults,
  periods with lots of faults, just one kind of fault, etc. This takes a time
  period in seconds, which is how long to emit nemesis operations for a
  particular subset of packages. Takes a collection of packages. Constructs a
  nemesis generator which emits faults for a shifting collection of packages
  over time."
  [period packages]
  ; We want a sequence of random subsets of packages
  (repeatedly
    (fn rand-pkgs []
      (let [; Pick packages
            pkgs (if (< (rand) 1/4)
                   ; Roughly 1/4 of the time, pick no pkgs
                    []
                    (package-gen-helper packages))
            ; Construct combined generators
            gen       (if (seq pkgs)
                        (apply gen/any (map :generator pkgs))
                        (gen/sleep period))
            final-gen (keep :final-generator pkgs)]
        ; Ops from the combined generator, followed by a final gen
        [(gen/log (str "Shifting to new mix of nemeses: "
                       (pr-str (map (comp n/fs :nemesis) pkgs))))
         (gen/time-limit period gen)
         final-gen]))))

;; File corruption

(def file-types
  "A map of file types (e.g. :blk) to regexes that identify them."
  ; TODO: figure out what data files ParticleDB uses, create types
  {:all #""})

(defn rand-data-file
  "Picks a random NATS data file on the given node. Uses `(:corrupt-file-type
  test)` to pick only some kinds of files."
  [test node]
  (c/with-node test node
             (c/su
               (->> (cu/ls db/data-dir
                           {:recursive? true
                            :full-path? true
                            :types [:file]})
                    (filter (if-let [t (:corrupt-file-type test)]
                              (partial re-find (file-types t))
                              (constantly true)))
                    vec
                    rand/nth-empty))))

(defn corrupt-file-expand-corruption
  "Fills in the parameters for a single file corruption operation's corruption"
  [test f {:keys [node] :as corruption}]
  ; Find a relevant file on the node
  (when-let [file (rand-data-file test node)]
    (when-let [size (try+ (-> (c/with-node test node
                                        (c/exec :stat "-c" "%s" file))
                             parse-long)
                          (catch [:type :jepsen.control/nonzero-exit] _))]
      (let [corruption (assoc corruption :file file)]
        (case f
          :bitflip-file-chunks
          (assoc corruption
                 :probability
                 ; Roughly two errors per file
                 (/ 2 8 size))

          :snapshot-file-chunks
          (assoc corruption :probability 0.5)

          :restore-file-chunks
          (assoc corruption :probability 0.5)

          :truncate-file
          (assoc corruption :size (- (rand/zipf size)))

          corruption)))))

(defn corrupt-file-expand-value
  "Fills in parameters for a file corruption operation."
  [{:keys [f value] :as op} test ctx]
  (update op :value (partial keep (partial corrupt-file-expand-corruption test f))))

(defn corrupt-file-package
  "A nemesis package for corrupting data files."
  [{:keys [faults interval corrupt-file] :as opts}]
  (let [{:keys []} corrupt-file
        ; What faults can we perform?
        faults (set/intersection faults
                                 #{:bitflip-file-chunks
                                   :snapshot-file-chunks
                                   :truncate-file})
        needed? (seq faults)
        ; Generator of core faults, without values
        gen (gen/mix
              (mapv {:bitflip-file-chunks
                     (gen/repeat {:type :info, :f :bitflip-file-chunks})

                     :snapshot-file-chunks
                     (gen/flip-flop
                       (gen/repeat {:type :info, :f :snapshot-file-chunks})
                       (gen/repeat {:type :info, :f :restore-file-chunks}))

                     :truncate-file
                     (gen/repeat {:type :info, :f :truncate-file})}
                    faults))
        ; Target a minority of nodes
        gen (nf/nodes-gen (comp util/minority count :nodes) gen)
        ;gen (nf/nodes-gen (constantly 1) gen)
        ; Expand values into specific files, probabilities, etc
        gen (gen/map corrupt-file-expand-value gen)
        ; And slow down
        gen (gen/stagger interval gen)]
    {:nemesis (nf/corrupt-file-nemesis
                ; 4K chunks by default, I guess?
                {:chunk-size (* 1024 4)})
     :generator (when needed? gen)
     :perf #{{:name "corrupt-file"
              :fs #{:bitflip-file-chunks
                    :copy-file-chunks
                    :snapshot-file-chunks
                    :restore-file-chunks
                    :truncate-file}
              :start  #{}
              :stop   #{}
              :color  "#D2E9A0"}}}))

(defn package
  "Takes CLI opts. Constructs a nemesis and generator for the test."
  [opts]
  (let [opts (update opts :faults set)
        packages
        (->> (concat
               ; Standard packages
               (nc/nemesis-packages opts)
               ; Custom packages
               [(corrupt-file-package opts)]))
        nsp (:stable-period opts)]
    ;(info :packages (map (comp n/fs :nemesis) packages))
    (cond-> (nc/compose-packages packages)
      nsp (assoc :generator (package-gen nsp packages)))))
