(ns validate-indent.core-test
  (:use midje.sweet)
  (:require [validate-indent.core :refer :all]
            ))

(fact
 (handle-char {:pstate :clojure, :extra-newlines [], :result []} \() => {:pstate :clojure, :extra-newlines [], :result [\(]}
 (handle-char {:pstate :clojure, :extra-newlines [], :result [\(]} \;) => {:pstate :comment, :extra-newlines [], :result [\(]}
 (reduce handle-char {:pstate :clojure, :extra-newlines [], :result []} "foo") => {:extra-newlines [], :pstate :clojure, :result [\f \o \o]}
 (handle-char {:pstate :clojure, :extra-newlines [], :result []} \tab) => {:pstate :clojure, :extra-newlines [], :result [\space \space]}
 (filter-out-paren "foo") => "foo"
 (filter-out-paren "(foo (bar ;asd\n  (asd);bar\n  ))") => "(foo (bar \n  (asd)\n  ))"
 (filter-out-paren "(foo (bar ;asd\n\"(asd);bar\n  \"))\n") => "(foo (bar \n))\n\n"
 )

(def test-data-multiline-string "asd\"
  asdasd
  asdadas\" adsasda
  asd")

(def test-data-multiline-string2
  "(def test-data \"(defproject cljtest \\\"0.1.0-SNAPSHOT\\\"\"")

(fact
 (filter-out-paren "\"fooo\"") => ""
 (filter-out-paren "\"fo\\\"oo\"") => ""
 (filter-out-paren "\"fo\noo\"") => ""
 (filter-out-paren test-data-multiline-string2) => "(def test-data "
 )

(def path-to-this-file
  (clojure.string/join "/" [
                            (-> (ClassLoader/getSystemResource *file*) clojure.java.io/file .getParent)
                            (last (clojure.string/split *file* #"/"))]))

(def test-data "(defproject cljtest \"0.1.0SNAPSHOT\"
  :description \"FIXME: write description\"
  :url \"http://example.com/FIXME\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :plugins [[lein-git-deps \"0.0.1-SNAPSHOT\"]]
  :dependencies [
                 [org.clojure/clojure \"1.5.1\"]
                 [bond \"0.2.5\"]
                 [midje \"1.6.0\"]
                 [org.clojure/tools.reader \"0.8.3\"]
                 ]
  :git-dependencies [[\"https://github.com/cgrand/sjacket.git\"]]
  :source-paths [\"src\", \".lein-git-deps/sjacket/src/\"]
  :main ^:skip-aot cljtest.core
  :target-path \"target/%s\"
  :profiles {:uberjar {:aot :all}})
")

(fact
 (filter-out-paren "(defproject cljtest \"0.1.0SNAPSHOT\"\n  :description \"FIXME: write description\"")
 => "(defproject cljtest \n  :description "

 ((lines test-data) 0) => "(defproject cljtest "


 (count-char \( "(( )) asd") => 2
 (count-char \a "(( )) asd") => 1
 (count-char \( ((lines test-data) 0)) => 1

 (empty-line? "   \n  ") => true
 (empty-line? "  a   ") => false

 (map count-lparen (lines test-data)) => '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

 (map indents-for-line (lines test-data)) => '(1 0 0 1 -1 0 1 0 0 0 0 -1 0 0 0 0 -1)

 (expected-indents-for-lines (lines test-data)) => '(0 1 1 1 2 1 1 2 2 2 2 2 1 1 1 1 1 0)

 (found-indents-for-line "") => :empty-line
 (found-indents-for-line "   ") => :empty-line
 (found-indents-for-lines (lines test-data)) => '(0 2 2 2 12 2 2 17 17 17 17 17 2 2 2 2 2 0)

 (zip [1 2 3] [4 5 6]) => '([1 4] [2 5] [3 6])
 (zip [1 2 3] [4 5 6 7 8]) => '([1 4] [2 5] [3 6])
 (zip [1 2 3 7 8 9] [4 5 6]) => '([1 4] [2 5] [3 6])

 (enumerate [:a :b :c]) => '([0 :a] [1 :b] [2 :c])

 (fill-empty-lines-to-the-right [1 2 :empty-line :empty-line 3]) => [1 2 2 2 3]
 (fill-empty-lines-to-the-right [1 2 :empty-line :empty-line :empty-line]) => [1 2 2 2 2]
 (handle-empty-lines [1 2 :empty-line :empty-line 3]) => [1 2 3 3 3]

 (bigger-than-prev [1 2 3]) => [false true true]
 (bigger-than-prev [3 2 1]) => [false false false]
 (bigger-than-prev [1 2 :empty-line 3]) => [false true true false]
 (bigger-than-prev [1 2 :empty-line :empty-line 3]) => [false true true false false]
 (bigger-than-prev (found-indents-for-lines (lines test-data))) => '(false true false false true false false true false false false false false false false false false false)
 (bigger-than-prev (expected-indents-for-lines (lines test-data)))  => '(false true false false true false false true false false false false false false false false false false)
 )

(def test-data-foo "(def test-data-multiline-string \"asd\\\"
  asdasd
  asdadas\\\" adsasda
  asd\")
")

(fact
 (find-indent-problems (lines test-data-foo)) => []
 )

(def test-data3 "(defproject cljtest \"0.1.0-SNAPSHOT

  \"
  :description \"FIXME: write description\"
  :url \"http://example.com/FIXME\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :plugins [[lein-git-deps \"0.0.1-SNAPSHOT\"]]
  :dependencies [
             [org.clojure/clojure \"1.5.1\"]
                 [bond \"0.2.5\"]
       [midje \"1.6.0\"]
                     [org.clojure/tools.reader \"0.8.3\"]
                 ]
  :git-dependencies [[\"https://github.com/cgrand/sjacket.git\"]]
  :source-paths [\"src\", \".lein-git-deps/sjacket/src/\"]
  :main ^:skip-aot cljtest.core
  :target-path \"target/%s\"
  :profiles {:uberjar {:aot :all}})
")

(fact
 (find-indent-problems (lines test-data3)) => [11 13]
 )

(def test-data2 (slurp path-to-this-file))

(fact
 (find-indent-problems (lines test-data2)) => []
 (found-indents-for-lines (lines "\n  \n")) => [:empty-line :empty-line 0]
 (expected-indents-for-lines (lines "\n  \n")) => [0 0 0]
 (find-indent-problems (lines "\n  \n")) => []

 (find-tab-problems ["\tfoo"]) => [1]
 (find-tab-problems ["" "\tfoo"]) => [2]
 )
