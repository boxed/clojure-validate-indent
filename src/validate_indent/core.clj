(ns validate-indent.core
  (:gen-class))

(def find-indent-problems)

(defn -main [& args]
  (if (= args nil)
    (print "Usage: lein run <file1> <file2>\n")
    (doseq [arg args]
      (let [indent-problems (find-indent-problems (slurp arg))]
        (if (not= (count indent-problems) 0)
          (print (format "%s: %s" arg (clojure.string/join ", " indent-problems)))))
      (print "\n"))))


(defn filter-out-paren [s]
  (loop [x (first s)
         xs (rest s)
         state :clojure
         extra-newlines []
         result []]
    (if (= x nil)
      (apply str result) ; we're done!
      (case state
        :clojure (case x
                   \\ (recur (first xs) (rest xs) :character-literal extra-newlines result)
                   \; (recur (first xs) (rest xs) :comment extra-newlines result)
                   \" (recur (first xs) (rest xs) :string-literal extra-newlines result)
                   (\( \) \[ \] \{ \} \space) (recur (first xs) (rest xs) state extra-newlines (conj result x))
                   \newline (recur (first xs) (rest xs) state [] (apply conj result x extra-newlines))
                   (recur (first xs) (rest xs) state extra-newlines (conj result x))
                   )
        :string-literal (case x
                          \\ (recur (first xs) (rest xs) :character-literal-in-string extra-newlines result)
                          \" (recur (first xs) (rest xs) :clojure extra-newlines result)
                          \newline (recur (first xs) (rest xs) state (conj extra-newlines \newline) result)
                          (recur (first xs) (rest xs) state extra-newlines result)
                          )
        :character-literal (recur (first xs) (rest xs) :clojure extra-newlines result)
        :character-literal-in-string (recur (first xs) (rest xs) :string-literal extra-newlines result)
        :comment (if (= x \newline)
                   (recur (first xs) (rest xs) :clojure extra-newlines (conj result x))
                   (recur (first xs) (rest xs) state extra-newlines result))
        (recur (first xs) (rest xs) state extra-newlines result)
        )
      )))

(defn count-char [ch s]
  (count (for [x s :when (= x ch)] 1)))


(defn lines [x]
  (clojure.string/split (filter-out-paren x) #"\n"))

(defn empty-line? [line]
  (= (clojure.string/trim line) ""))

(def count-lparen (partial count-char \())
(def count-rparen (partial count-char \)))

(def count-lbracket (partial count-char \[))
(def count-rbracket (partial count-char \]))

(def count-lbrace (partial count-char \{))
(def count-rbrace (partial count-char \}))

(defn indents-for-line [line]
  (- (+ (count-lparen line) (count-lbracket line) (count-lbrace line))
     (+ (count-rparen line) (count-rbracket line) (count-rbrace line))))

(defn expected-indents-for-lines [lines]
  (let [indents (map indents-for-line lines)]
    (concat '(0)
      (for [i (range (count lines))]
        (reduce + (take (+ i 1) indents))))))

(defn found-indents-for-line [line]
  (if (empty-line? line)
    :empty-line
    (count (take-while (partial = \space) line))))

(defn found-indents-for-lines [lines]
  (map found-indents-for-line lines))

(defn zip
  [& colls]
  (apply map vector colls))

(defn enumerate [x]
  (zip (range (count x)) x))


(defn fill-empty-lines-to-the-right [xs]
  (for [i (range (count xs))]
    (last (filter (partial not= :empty-line) (take (+ i 1) xs))))
  )

(defn handle-empty-lines [xs]
  (reverse (fill-empty-lines-to-the-right (reverse xs))))

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
