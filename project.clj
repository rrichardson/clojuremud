(defproject clojuremud ""

  :description "Minimum Viable MUD"
  :url "http://github.com/rrichardson/clojuremud"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.5.1"]

                 [ring/ring-core "1.2.0"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]
                 [jarohen/chord "0.2.2"]
                 [crypto-random "1.1.0"]

                 [org.clojure/tools.reader "0.8.2"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/core.match "0.2.0"]

                 [prismatic/dommy "0.1.1"]

                 [org.clojure/clojurescript "0.0-2014"]
                 [org.clojure/tools.reader "0.7.10"]
                 [org.flatland/jiraph "0.12.3-alpha1"]
                 ;;[clojurewerkz/titanium "1.0.0-beta1"]
                 [clojail "1.0.6"]
                 [serializable-fn "1.1.3"]
                 ]

  :plugins [[jarohen/lein-frodo "0.2.3"]
            [lein-cljsbuild "1.0.0-alpha2"]
            [lein-pdo "0.1.1"]]

  :frodo/config-resource "clojuremud-config.edn"

  :source-paths ["src/clojure"]

  :resource-paths ["resources" "target/resources"]

  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "resources/js/clojuremud-client.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}

  :aliases {"dev" ["pdo" "cljsbuild" "auto," "frodo"]
            "start" ["do" "cljsbuild" "once," "trampoline" "frodo"]})

