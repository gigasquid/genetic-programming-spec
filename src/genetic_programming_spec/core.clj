(ns genetic-programming-spec.core
  (:require [clojure.spec :as s]))


(def seqs ['s/+ 's/*])
(def preds ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seq-prob 0.5)
(def nest-prob 0.0)
(def max-depth 4)
(def max-num 5)

(declare make-random-seq)

(defn make-random-arg [n]
  (if (and (pos? n) (> (rand) seq-prob))
    `~(make-random-seq n)
    `~(rand-nth preds)))

(defn make-random-seq [n]
  (if (< (rand) nest-prob)
    `(s/spec ~(rand-nth seqs) ~(make-random-arg (dec n)))
    `(~(rand-nth seqs) ~(make-random-arg (dec n)))))

(make-random-arg 2)

(defn make-random-cat [len]
  (let [args (reduce (fn [r i]
                       (conj r (keyword (str i)) (make-random-arg max-depth))) [] (range len))]
    `(s/cat ~@args)))

(def x (make-random-cat 2))
x

(s/explain-data (eval x) [true 2])
(get-in  (:clojure.spec/problems (s/explain-data (eval x) [true 1])) [0 :in 0]
     )

(defn initial-population [popsize cat-len]
  (for [i (range popsize)]
    {:program (make-random-cat cat-len)}))

(def pop (initial-population 5 4))

(defn score [creature test-data]
  (let [problems (:clojure.spec/problems (s/explain-data (eval (:program creature)) test-data))]
    (if problems
      (assoc creature :score (get-in problems [0 :in 0]))
      (assoc creature :score 100))))

(score {:program x} [1 1])



 (s/conform even? 100)

  (s/conform (s/cat :x even? :y odd?) [2 4])

(s/explain (s/alt :even even? :small #(< % 42)) 50)




(s/explain (s/alt :s string? :b boolean?) [false])
(s/explain (s/* string?) ["jo" "i" ])
(s/explain (s/+ string?) ["jo" "i" ])
(s/conform (s/? string?) ["jo" ])

(s/explain (s/cat :x (s/spec (s/+ string?)) :y (s/+ int?)) [["a" "b"] 1 3])
(s/explain-data (s/cat :1 (s/+ string?) :2 (s/+ int?)) ["a" "b" "c" true])


