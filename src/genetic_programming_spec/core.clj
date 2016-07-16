(ns genetic-programming-spec.core
  (:require [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.walk :as walk]))


(def seqs ['s/+ 's/*])
(def preds ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seq-prob 0.5)
(def nest-prob 0.05)
(def mutate-prob 0.1)
(def crossover-prob 0.7)
(def max-depth 4)
(def max-cat-len 5)

(declare make-random-seq)

(defn make-random-arg [n]
  (if (and (pos? n) (< (rand) seq-prob))
    `~(make-random-seq n)
    `~(rand-nth preds)))

(defn make-random-seq [n]
  (if (< (rand) nest-prob)
    `(s/spec (~(rand-nth seqs) ~(make-random-arg (dec n))))
    `(~(rand-nth seqs) ~(make-random-arg (dec n)))))

(make-random-arg 2)

(defn make-random-cat [len]
  (let [args (reduce (fn [r i]
                       (conj r (keyword (str i)) (make-random-arg max-depth))) [] (range len))]
    `(s/cat ~@args)))

(defn score [creature test-data]
  (try
   (let [problems (:clojure.spec/problems (s/explain-data (eval (:program creature)) test-data))]
     (if problems
       (assoc creature :score (get-in problems [0 :in 0]))
       (assoc creature :score 100)))
   (catch Throwable e (do (println "Creature is bad!" e)
                          (assoc creature :score 0)))))

(defn mutable? [node]
  (or (when (seq? node)
        (contains? (set/union (set seqs) #{'clojure.spec/spec}) (first node)))
      (contains? (set preds) node)))

(defn mutate [creature]
  (let [program (:program creature)

        mutated-program (walk/postwalk (fn [x] (if (and (mutable? x) (< (rand) mutate-prob))
                                                (make-random-arg max-depth)
                                                x)) program)]
    (assoc creature :program mutated-program)))

(defn crossover [creature1 creature2]
  (let [program1 (:program creature1)
        program2 (:program creature2)
        chosen-node (first (walk/walk #(when (and (< (rand) crossover-prob) (mutable? %)) %)
                                       #(remove nil? %) program1))
        crossed-over? (atom false)
        crossover-program (if chosen-node
                             (walk/postwalk (fn [x] (if (and (mutable? x) (< (rand) crossover-prob) (not @crossed-over?))
                                                (do (reset! crossed-over? true) chosen-node)
                                                x)) program2)
                             program2)]
    {:program crossover-program}))

(defn initial-population [popsize]
  (for [i (range popsize)]
    {:program (make-random-cat (inc (rand-int max-cat-len)))}))

(def population (initial-population 5))

population


(comment
  (def x (make-random-cat 4))
  x
  (s/explain-data (eval x) [true 2 true false])
  (score {:program x} [1 1 nil 1])
  (mutate {:program x :score 1})
  (crossover {:program x :score 1} {:program (make-random-cat 4) :score 1})

)








