(defproject edn-query-language/eql "1.0.1"
  :description "EDN Query Language support library"
  :url "https://github.com/edn-query-language/eql"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.764" :scope "provided"]
                 [org.clojure/test.check "1.1.0"]]

  :jar-exclusions [#"resources/.*" #"node-modules/.+" #"public/.*" #"\.DS_Store"]

  :deploy-repositories [["clojars" {:url   "https://clojars.org/repo/"
                                    :creds :gpg :checksum :ignore}]
                        ["releases" :clojars]
                        ["snapshots" :clojars]])
