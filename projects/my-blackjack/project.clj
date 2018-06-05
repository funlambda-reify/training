(defproject blackjack "0.1.0-SNAPSHOT"
  :description "Basic command-line blackjack application for Clojure practice"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot blackjack.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
