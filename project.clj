;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(defproject uncomplicate/commons "0.12.3"
           :description "Common Uncomplicate utilities."
  :url "https://github.com/uncomplicate/commons"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/commons"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]]

  :codox {:src-dir-uri "http://github.com/uncomplicate/commons/blob/master/"
          :src-linenum-anchor-prefix "L"
          :output-path "docs/codox"}

  :profiles {:dev {:plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.7"]]
                   :global-vars {*warn-on-reflection* true
                                 *assert* true
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :dependencies [[midje "1.10.4"]]
                   :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
                                        "--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED"
                                        "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"]}}

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"])
