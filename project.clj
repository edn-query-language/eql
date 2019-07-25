(defproject edn-query-language/eql "0.0.8"
  :description "EDN Query Language support library"
  :url "https://github.com/edn-query-language/eql"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}

  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :jar-exclusions [#"public/.*" #"\.DS_Store"])
