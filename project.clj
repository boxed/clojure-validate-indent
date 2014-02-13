(defproject validate-indent "0.1.0"
  :description "Validate the indenting of your clojure code to make sure they are in sync with the parenthesis"
  :url "https://github.com/boxed/clojure-validate-indent"
  :license {:name "Public domain"}
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [midje "1.6.0"]
                 ]
  :source-paths ["src"]
  :main ^:skip-aot validate-indent.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
