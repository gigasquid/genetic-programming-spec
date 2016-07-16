(ns genetic-programming-spec.core
  (:require [clojure.spec :as s]
            [clojure.set :as set]
            [clojure.walk :as walk]))


(def seqs ['s/+ 's/*])
(def preds ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seq-prob 0.5)
(def nest-prob 0.0)
(def mutate-prob 0.1)
(def max-depth 4)
(def max-num 5)

(declare make-random-seq)

(defn make-random-arg [n]
  (if (and (pos? n) (< (rand) seq-prob))
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

(def x (make-random-cat 4))
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


(defn mutate [creature]
  (let [program (:program creature)
        mutable? (fn [x] (contains? (set/union (set seqs) (set preds)) x))
        mutated-program (walk/postwalk (fn [x] (if (and (mutable? x) (< (rand) mutate-prob))
                                                (make-random-arg max-depth)
                                                x)) x)]
    (assoc creature :program mutated-program)))

(mutate {:program x :score 1})






