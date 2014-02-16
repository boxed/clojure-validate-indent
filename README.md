Validate Indent
===============

`validate-indent` is a clojure program to validate indents in clojure source code. Example:

For this file:

```clojure
(defn foo [a b c]
  (if (and (= a 1) (= b 3)))
    (prn "foo")
    (prn "bar")
```

Will give you an error on line 3 because there's an indent without a corresponding opening brace/paren.


To look at a bit more complex example clone the repo and run it with `lein run test/file_with_bad_indents.clj` will give the output `test/file_with_bad_indents.clj: 12, 16, 23` to show that those line numbers are the ones I screwed up the indenting on to create the example :P
