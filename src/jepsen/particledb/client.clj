(ns jepsen.particledb.client
  "Wraps the ParticleDB JDBC client"
  (:require [clojure [pprint :refer [pprint]]]
            [clojure.tools.logging :refer [info warn]]
            [clj-commons.slingshot :refer [try+ throw+]]
            [dom-top.core :refer [loopr]]
            [jepsen.postgres.client :as pg]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql.builder :as sqlb]
            [potemkin :refer [import-vars]]
            [slingshot.slingshot :as s :refer [throw+]])
  (:import (java.sql SQLException)
           (org.particledb Driver)))


; We'll just borrow these from the postgres client wrapper
(import-vars [jepsen.postgres.client close! with-txn with-manual-txn])

(def port
  "The ParticleDB JDBC driver port."
  5440)

(defn open
  "Opens a JDBC connection to the given node."
  [test node]
  (let [spec {:dbtype "particledb"
              ; First it was com.particledb, then ai.particledb, now
              ; org.particledb??? I feel like an LLM is probably responsible
              ; for this nonsense...
              :classname "org.particledb.Driver"
              :host node
              :port port}]
    (->> spec
         j/get-datasource
         j/get-connection
         (pg/with-logging test))))

(defn ^:dynamic *known-error*
  "Turns some types of Exception into a throwable map. This is meant to be
  re-bound by callers so that they can handle the types of exceptions their
  particular client/DB throws."
  [e]
  (let [m (.getMessage e)]
    (condp identical? (class e)
      SQLException
      (condp re-find m
        #"duplicate key value" {:type :duplicate
                                :definite? true}))))

(defmacro with-errors
  "Evaluates body, converting some errors into thrown Slingshot maps using
  `*known-error*`."
  [& body]
  `(try ~@body
        (catch Exception e#
          (if-let [known# (*known-error* e#)]
            (throw+ known#)
            (throw e#)))))

(defmacro try+
  "Like Slingshot try+, but rewrites exceptions using *known-error*. This
  way, you can write:

      (try+ (j/execute [...])
        (catch [:definite? true] ...))"
  [known-error & forms]
  (let [; Split up into body expressions and catch/finally clauses
        parts (group-by (fn [form]
                          (boolean (and (seq? form)
                                        (#{'catch 'finally} (first form)))))
                        forms)
        body (parts false)
        clauses (parts true)]
    (pprint parts)
    (assert (seq body) "No body for try+")
    (assert (seq clauses) "No catch/finally clauses for try+")
    `(s/try+ (with-errors ~@body)
             ~@clauses)))
