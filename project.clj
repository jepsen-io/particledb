(defproject io.jepsen/particledb "0.1.0-SNAPSHOT"
  :description "Jepsen tests for the ParticleDB SQL database."
  :url "https://github.com/jepsen/particledb"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [jepsen "0.3.12-SNAPSHOT"
                  :exclusions [com.fasterxml.jackson.core/jackson-core]]
                 [io.jepsen/sql "0.1.0-SNAPSHOT"]
                 [io.jepsen/postgres "0.1.2-SNAPSHOT"
                  :exclusions [jepsen]]
                 [ai.particledb/particledb-jdbc "1.0.0"
                  :exclusions [org.slf4j/slf4j-api]]
                 [com.gfredericks/test.chuck "0.2.15"]
                 [com.aphyr/hegel-clj "0.1.0-SNAPSHOT"]
                 ; The particledb jdbc driver literally cannot execute a SELECT
                 ; unless you manually pull in Apache Arrow. What is going ON
                 ; here???
                 [org.apache.arrow/arrow-vector "15.0.2"
                  :exclusions [org.slf4j/slf4j-api
                               com.fasterxml.jackson.core/jackson-core
                               com.fasterxml.jackson.core/jackson-databind
                               com.fasterxml.jackson.core/jackson-annotations]]
                 [org.apache.arrow/arrow-memory-netty "15.0.2"
                  :exclusions [org.slf4j/slf4j-api]]
                 ; Just for the repl's repro watcher
                 [juxt/dirwatch "0.2.5"
                  :exclusions [org.slf4j/slf4j-api]]
                 [clojure-term-colors "0.1.0"]]
  :repl-options {:init-ns jepsen.particledb.repl}
  :main jepsen.particledb.cli
  :aot [jepsen.particledb.cli]
  :jvm-opts ["-Djava.awt.headless=true"
             ; ParticleDB doesn't bind to v6 addresses, I think
             "-Djava.net.preferIPv4Stack=true"
             "-server"
             ; OK, sure, why not
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
             ; Our test.check generators need a LOT of stack
             "-Xss16m"
             ]
  :profiles {:uberjar {:aot :all}})
