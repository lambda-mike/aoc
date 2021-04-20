(defproject day13 "1.0"
  :description "Advent of Code 2020 Day13"
  :url "https://github.com/lambda-mike/aoc"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main day13.core
  :target-path "target/%s"
  :keep-non-project-classes true
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
