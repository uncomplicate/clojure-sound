(defproject org.uncomplicate/clojure-sound "0.3.0"
  :description "Clojure Sound is a library for transforming digital media and communicating with MIDI devices."
  :url "https://github.com/uncomplicate/clojure-sound"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/clojure-sound"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [uncomplicate/commons "0.13.0"]]
  :profiles {:dev {:plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.7"]]
                   :global-vars {*warn-on-reflection* true
                                 *assert* false
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :dependencies [[midje "1.10.5"]
                                  [codox-theme-rdash "0.1.2"]]
                   :codox {:metadata {:doc/format :markdown}
                           :source-uri "http://github.com/uncomplicate/clojure_sound/blob/master/{filepath}#L{line}"
                           :themes [:rdash]
                           :output-path "docs/codox"}}}

  :test-paths ["test/clojure" "test/resources"])
