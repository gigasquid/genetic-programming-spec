(ns genetic-programming-spec.core
  (:require [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.walk :as walk]))


(def seqs ['s/+ 's/*])
(def and-ors ['s/and 's/or])
(def preds ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seq-prob 0.3)
(def nest-prob 0.00)
(def and-or-prob 0.5)
(def mutate-prob 0.1)
(def crossover-prob 0.7)
(def new-node-prob 0.05)
(def max-depth 4)

(declare make-random-seq)

(defn make-random-arg [n]
  (if (and (pos? n) (< (rand) seq-prob))
    `~(make-random-seq n)
    `~(rand-nth preds)))

(defn make-random-seq [n]
  (cond

    (< (rand) nest-prob)
    `(s/spec (~(rand-nth seqs) ~(make-random-arg (dec n))))


    (< (rand) and-or-prob)
    `(~(rand-nth and-ors) ~(make-random-arg (dec n)))

    :else
    `(~(rand-nth seqs) ~(make-random-arg (dec n)))))


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
   (catch Throwable e (assoc creature :score 0))))

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

(defn initial-population [popsize max-cat-length]
  (for [i (range popsize)]
    {:program (make-random-cat (inc (rand-int max-cat-length)))}))


(defn select-best [creatures tournament-size]
  (let [selected (repeatedly tournament-size #(rand-nth creatures))]
    (-> (sort-by :score selected) reverse first)))

(defn perfect-fit [creatures]
  (first (filter #(= 100 (:score %)) creatures)))

(defn evolve [pop-size max-gen tournament-size test-data]
  (loop [n max-gen
         creatures (initial-population pop-size (count test-data))]
    (println "generation " (- max-gen n))
    (let [scored-creatures (map (fn [creature] (score creature test-data)) creatures)]
     (if (or (zero? n) (perfect-fit scored-creatures))
       scored-creatures
       (let [elites (take 2 (reverse (sort-by :score scored-creatures)))
             new-creatures (for [i (range (- (count creatures) 2))]
                             ;; add a random node to improve diversity
                             (if (< (rand) new-node-prob)
                               {:program (make-random-cat (count test-data))}
                               (let [creature1 (select-best scored-creatures tournament-size)
                                     creature2 (select-best scored-creatures tournament-size)]
                                 (mutate (crossover creature1 creature2)))))]
         (println "best-scores" (map :score elites))
         (recur (dec n) (into new-creatures elites)))))))


(comment

  (def creature-specs (evolve 100 100 7 ["hi" true 5 10 "boo"]))
  (perfect-fit creature-specs)
  ;{:program (clojure.spec/cat :0 string? :1 boolean? :2 (s/and integer? odd?) :3 integer? :4 string?), :score 100}


  (s/exercise (eval (:program (perfect-fit creature-specs))) 5)
;; ([("" true -1 -1 "") {:0 "", :1 true, :2 -1, :3 -1, :4 ""}]
;;  [("D" false -1 -1 "G") {:0 "D", :1 false, :2 -1, :3 -1, :4 "G"}]
;;  [("12" false -1 0 "l0") {:0 "12", :1 false, :2 -1, :3 0, :4 "l0"}]
;;  [("" false -1 -2 "") {:0 "", :1 false, :2 -1, :3 -2, :4 ""}]
;;  [("2" false 1 0 "Jro") {:0 "2", :1 false, :2 1, :3 0, :4 "Jro"}])


)








