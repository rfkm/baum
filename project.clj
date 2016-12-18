(defproject rkworks/baum "0.4.0"
  :description "Extensible EDSL in EDN for building self-contained configuration files"
  :url "https://github.com/rfkm/baum"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :deploy-repositories [["snapshots" {:url      "https://clojars.org/repo/"
                                      :username [:gpg :env]
                                      :password [:gpg :env]}]
                        ["releases" {:url   "https://clojars.org/repo/"
                                     :creds :gpg}]]
  :dependencies [[environ "1.1.0"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.reader "1.0.0-alpha2"]]
  :profiles {:dev    {:dependencies [[org.clojure/clojure "1.8.0"]
                                     [midje "1.8.3"]]
                      :plugins      [[lein-midje "3.2.1"]
                                     [lein-environ "1.1.0"]]
                      :env          {:env "dev"}}
             :1.5    {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6    {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7    {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8    {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :master {:dependencies [[org.clojure/clojure "1.9.0-master-SNAPSHOT"]]}}
  :aliases {"all" ["with-profile" "+1.5:+1.6:+1.7:+1.8:+master"]})
