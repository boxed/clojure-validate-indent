(ns validate-indent.core-test
  (:use midje.sweet)
  (:require [validate-indent.core :refer :all]
            ))

(defn filter-out-paren [s]
 (let [result (ref [])
       state (ref :clojure)
       extra-newlines (ref 0)]
  (dosync
    (doseq [x s]
      (case @state
        :clojure (case x
                   \\ (ref-set state :character-literal)
                   \; (ref-set state :comment)
                   \" (ref-set state :string-literal)
                   (\( \) \[ \] \{ \} \space) (alter result conj x)
                   \newline (do ; All this stuff it to make sure the line numbers don't get shifted by multiline string literals
                              (doseq [l (range @extra-newlines)]
                                (alter result conj \newline))
                              (ref-set extra-newlines 0)
                              (alter result conj x))
                   (alter result conj x)
                   )
        :string-literal (case x
                          \\ (ref-set state :character-literal-in-string)
                          \" (ref-set state :clojure)
                          \newline (alter extra-newlines + 1)
                          nil)
        :character-literal (ref-set state :clojure)
        :character-literal-in-string (ref-set state :string-literal)
        :comment (if (= x \newline)
                   (do
                     (ref-set state :clojure)
                     (alter result conj x)))
        )
      )
    (ref-set result (apply str @result)))
  @result))



(fact
 (filter-out-paren "(foo (bar ;asd\n\"(asd);bar\n  \"))\n") => "(foo (bar \n))\n\n"
 (filter-out-paren "(foo (bar ;asd\n  (asd);bar\n  ))") => "(foo (bar \n  (asd)\n  ))"
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

(defn count-char [ch s]
  (count (for [x s :when (= x ch)] 1)))


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

(defn lines [x]
  (clojure.string/split (filter-out-paren x) #"\n"))

(fact
 (filter-out-paren "(defproject cljtest \"0.1.0SNAPSHOT\"\n  :description \"FIXME: write description\"")
 => "(defproject cljtest \n  :description "

 ((lines test-data) 0) => "(defproject cljtest "
 )

(fact
 (count-char \( "(( )) asd") => 2
 (count-char \a "(( )) asd") => 1
 (count-char \( ((lines test-data) 0)) => 1
 )

(defn empty-line? [line]
  ;(= (clojure.string/replace line #" *(;.*)?" "") ""))
  (= (clojure.string/trim line) ""))

(fact
 (empty-line? "   \n  ") => true
 (empty-line? "  a   ") => false
 )


(def count-lparen (partial count-char \())
(def count-rparen (partial count-char \)))

(def count-lbracket (partial count-char \[))
(def count-rbracket (partial count-char \]))

(def count-lbrace (partial count-char \{))
(def count-rbrace (partial count-char \}))

(fact
 (map count-lparen (lines test-data)) => '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 )

(defn indents-for-line [line]
  (- (+ (count-lparen line) (count-lbracket line) (count-lbrace line))
     (+ (count-rparen line) (count-rbracket line) (count-rbrace line))))

(fact
 (map indents-for-line (lines test-data)) => '(1 0 0 1 -1 0 1 0 0 0 0 -1 0 0 0 0 -1)
 )

(defn expected-indents-for-lines [lines]
  (let [indents (map indents-for-line lines)]
    (concat '(0)
      (for [i (range (count lines))]
        (reduce + (take (+ i 1) indents))))))

(fact
 (expected-indents-for-lines (lines test-data)) => '(0 1 1 1 2 1 1 2 2 2 2 2 1 1 1 1 1 0)
 )

(defn found-indents-for-line [line]
  (if (empty-line? line)
    :empty-line
    (count (take-while (partial = \space) line))))

(defn found-indents-for-lines [lines]
  (map found-indents-for-line lines))

(fact
 (found-indents-for-line "   ") => :empty-line
 (found-indents-for-lines (lines test-data)) => '(0 2 2 2 12 2 2 17 17 17 17 17 2 2 2 2 2)
 )

(defn zip
  [& colls]
  (apply map vector colls))

(fact
 (zip [1 2 3] [4 5 6]) => '([1 4] [2 5] [3 6])
 )

(defn enumerate [x]
  (zip (range (count x)) x))

(fact
 (enumerate [:a :b :c]) => '([0 :a] [1 :b] [2 :c])
 )


(defn fill-empty-lines-to-the-right [xs]
  (for [i (range (count xs))]
    (last (filter (partial not= :empty-line) (take (+ i 1) xs))))
  )

(defn handle-empty-lines [xs]
  (reverse (fill-empty-lines-to-the-right (reverse xs))))

(fact
 (fill-empty-lines-to-the-right [1 2 :empty-line :empty-line 3]) => [1 2 2 2 3]
 (handle-empty-lines [1 2 :empty-line :empty-line 3]) => [1 2 3 3 3]
 )

(defn bigger-than [p]
  (let [x (p 0)
        y (p 1)]
      (> x y)))

(defn non-empty-line-after [index xs]
  (let [splice (drop (+ index 1) xs)]
    (prn splice)
    (for [x splice :when (not= x :empty-line)] x)))

(defn bigger-than-prev [xs-in]
  (let [xs (handle-empty-lines xs-in)]
    (concat '(false) (for [w (zip (drop 1 xs) xs)]
                       (bigger-than w)))))


(fact
 (bigger-than-prev [1 2 3]) => [false true true]
 (bigger-than-prev [3 2 1]) => [false false false]
 (bigger-than-prev [1 2 :empty-line 3]) => [false true true false]
 (bigger-than-prev [1 2 :empty-line :empty-line 3]) => [false true true false false]
 (bigger-than-prev (found-indents-for-lines (lines test-data))) => '(false true false false true false false true false false false false false false false false false)
 (bigger-than-prev (expected-indents-for-lines (lines test-data)))  => '(false true false false true false false true false false false false false false false false false false)
 )

(defn find-indent-problems [test-data]
  (let [found (found-indents-for-lines (lines test-data))
        expected (expected-indents-for-lines (lines test-data))
        x (bigger-than-prev found)
        y (bigger-than-prev expected)]
    (for [w (enumerate (zip x y)) :when (or
                                         (apply not= (w 1))
                                         (= ((w 1) 1) :empty-line))]
      (let [line-number (+ (w 0) 1)] ; Add one to make it 1-based indexes
        line-number))))

(def test-data-foo "(def test-data-multiline-string \"asd\\\"
  asdasd
  asdadas\\\" adsasda
  asd\")
")

(fact
 (find-indent-problems test-data-foo) => []
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
 (find-indent-problems test-data3) => [11 13]
 )

(def test-data2 (slurp path-to-this-file))

(fact
 (find-indent-problems test-data2) => []
 )

