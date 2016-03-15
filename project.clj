(defproject uncomplicate/commons "0.1.0"
  :description "Common Uncomplicate utillities."
  :url "https://github.com/uncomplicate/commons"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/commons"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]

  :codox {:src-dir-uri "http://github.com/uncomplicate/commons/blob/master/"
          :src-linenum-anchor-prefix "L"
          :output-path "docs/codox"}

  :profiles {:dev {:plugins [[lein-midje "3.1.3"]
                             [lein-codox "0.9.4"]]
                   :global-vars {*warn-on-reflection* true
                                 *assert* true
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :dependencies [[midje "1.8.3"]
                                  [criterium "0.4.4"]]}}

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"])
