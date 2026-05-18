(ns jepsen.particledb.client
  "Wraps the ParticleDB JDBC client."
  (:require [clojure.tools.logging :refer [info warn]]
            [clj-commons.slingshot :refer [try+ throw+]]))

(def pg-port
  "The PostgreSQL port."
  5432)
