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
