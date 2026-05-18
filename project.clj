(defproject io.jepsen/particledb "0.1.0-SNAPSHOT"
  :description "Jepsen tests for the ParticleDB SQL database."
  :url "https://github.com/jepsen/particledb"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [jepsen "0.3.12-SNAPSHOT"
                  :exclusions [com.fasterxml.jackson.core/jackson-core]]
                 [io.jepsen/postgres "0.1.2-SNAPSHOT"
                  :exclusions [jepsen]]]
  :repl-options {:init-ns jepsen.particledb.repl}
  :main jepsen.particledb.cli
  :jvm-opts ["-Djava.awt.headless=true"
             "-server"])
