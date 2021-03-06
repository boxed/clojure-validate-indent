(ns validate-indent.core
  (:gen-class))

(def find-indent-problems)
(def lines)

(defn print-indent-problems-for-file [file]
  (let [content (slurp file)
        lines (lines content)]
    (let [indent-problems (find-indent-problems lines)]
      (if (not= (count indent-problems) 0)
        (print (format "%s: %s\n" file (clojure.string/join ", " indent-problems)))))))

(defn -main [& args]
  (if (= args nil)
    (print "Usage: lein run <file1> <file2>\n")
    (doseq [arg args]
      (let [f (clojure.java.io/file arg)]
        (if (.isDirectory f)
          (doseq [file (file-seq f)]
            (if (and (.isFile file) (.endsWith (.getName file) ".clj"))
              (print-indent-problems-for-file file)))
          (print-indent-problems-for-file arg))))))


(defmulti handle-char (fn [{pstate :pstate} _] pstate))

(defmethod handle-char :clojure [{:keys [extra-newlines] :as st} ch]
  (case ch
    \\ (assoc (update-in st [:result] conj \_) :pstate :character-literal)  ; Replacing literal with _ to get correct indent for lines that start with a character literal
    \; (assoc st :pstate :comment)
    \" (assoc (update-in st [:result] conj \_) :pstate :string-literal) ; Replacing " with _ to get correct indent for lines that start with a string literal
    \tab (update-in (update-in st [:result] conj \space) [:result] conj \space) ; Interpret tab as two spaces
    \newline (-> st
                 (assoc :extra-newlines [])
                 (update-in [:result] (partial apply conj) ch extra-newlines))
    (update-in st [:result] conj ch)))

(defmethod handle-char :string-literal [st ch]
  (case ch
    \\ (assoc st :pstate :character-literal-in-string)
    \" (assoc st :pstate :clojure)
    \newline (update-in st [:extra-newlines] conj \newline)
    st))

(defmethod handle-char :character-literal [st ch]
  (assoc st :pstate :clojure))

(defmethod handle-char :character-literal-in-string [st ch]
  (assoc st :pstate :string-literal))

(defmethod handle-char :comment [{:keys [extra-newlines result] :as st} ch]
  (if (= ch \newline)
   (-> st (assoc :pstate :clojure) (update-in [:result] conj ch))
   st))

(defn filter-out-paren [s]
  (apply str (:result (reduce handle-char {:pstate :clojure, :extra-newlines [], :result []} s))))

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
  (concat (map found-indents-for-line lines) [0]))

(defn zip [& colls]
  (apply map vector colls))

(defn enumerate [x]
  (zip (range (count x)) x))

(defn fill-empty-lines-to-the-right [xs]
  (for [i (range (count xs))]
    (last (filter (partial not= :empty-line) (take (+ i 1) xs))))
  )

(defn handle-empty-lines [xs]
  (reverse (fill-empty-lines-to-the-right (reverse xs))))

(defn bigger-than [[x y]]
  (> x y))

(defn bigger-than-prev [xs-in]
  (let [xs (handle-empty-lines xs-in)]
    (concat '(false) (for [w (zip (drop 1 xs) xs)]
                       (bigger-than w)))))

(defn find-indent-problems [lines]
  (let [found (found-indents-for-lines lines)
        expected (expected-indents-for-lines lines)
        x (bigger-than-prev found)
        y (bigger-than-prev expected)]
    (for [[i w] (enumerate (zip x y)) :when (or (apply not= w) (= (w 1) :empty-line))]
      (let [line-number (+ i 1)] ; Add one to make it 1-based indexes
        line-number))))
