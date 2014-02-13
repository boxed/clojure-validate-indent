Validate Indent
===============

`validate-indent` is a clojure program to validate indents in clojure source code. Example:

We have a file that looks like this:

```clojure
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
           \" (ref-set state :string-literal)  ; INCORRECT INDENT!
                   (\( \) \[ \] \{ \} \space) (alter result conj x)
                   \newline (do 
                              (doseq [l (range @extra-newlines)]
                                (alter result conj \newline))
                                    (ref-set extra-newlines 0)
                              (alter result conj x))
                   (alter result conj x)
         )
        :string-literal (case x
                          \\ (ref-set state :character-literal-in-string)
                  \" (ref-set state :clojure)  ; INCORRECT INDENT!
                          \newline (alter extra-newlines + 1)
                          nil)
        :character-literal (ref-set state :clojure)
        :character-literal-in-string (ref-set state :string-literal)
  :comment (if (= x \newline)  ; INCORRECT INDENT!
                   (do
                     (ref-set state :clojure)
                     (alter result conj x)))
        )
      )
    (ref-set result (apply str @result)))
  @result))
```

Running `lein run test/file_with_bad_indents.clj` will give the output `test/file_with_bad_indents.clj: 12, 16, 23` to show that those line numbers are the ones I screwed up the indenting on to create the example :P
