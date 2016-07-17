# genetic-programming-spec

An experiment in genetic programming with clojure.spec

## Usage

Use the `evolve` function to generate clojure.spec programs from your test data.

Parameters are:
* population size
* max number of generations
* tournament size for selection
* vector of test data

```clojure
(def creature-specs (evolve 100 100 7 ["hi" true 5 10 "boo"]))
(perfect-fit creature-specs)
;=> {:program (clojure.spec/cat :0 string? :1 boolean? :2 (s/and integer? odd?) :3 integer? :4 string?), :score 100}
```

After you have your clojure.spec creature generated, you can have it generate even _more_ data for you!

```clojure
(s/exercise (eval (:program (perfect-fit creature-specs))) 5)
;; ([("" true -1 -1 "") {:0 "", :1 true, :2 -1, :3 -1, :4 ""}]
;;  [("D" false -1 -1 "G") {:0 "D", :1 false, :2 -1, :3 -1, :4 "G"}]
;;  [("12" false -1 0 "l0") {:0 "12", :1 false, :2 -1, :3 0, :4 "l0"}]
;;  [("" false -1 -2 "") {:0 "", :1 false, :2 -1, :3 -2, :4 ""}]
;;  [("2" false 1 0 "Jro") {:0 "2", :1 false, :2 1, :3 0, :4 "Jro"}])
```


## License

Copyright © 2016 Carin Meier

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
