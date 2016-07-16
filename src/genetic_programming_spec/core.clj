(ns genetic-programming-spec.core
  (:require [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.walk :as walk]))


(def seqs ['s/+ 's/*])
(def preds ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seq-prob 0.5)
(def nest-prob 0.00)
(def mutate-prob 0.1)
(def crossover-prob 0.7)
(def new-node-prob 0.05)
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

(defn evolve [pop-size max-gen tournament-size test-data]
  (loop [n max-gen
         creatures (initial-population pop-size (count test-data))]
    (println "generation " (- max-gen n))
    (if (zero? n)
      (map (fn [creature] (score creature test-data)) creatures)
      (let [scored-creatures (map (fn [creature] (score creature test-data)) creatures)
            elites (take 2 (reverse (sort-by :score scored-creatures)))
            new-creatures (for [i (range (- (count creatures) 2))]
                            ;; prob add a totally new node
                            (if (< (rand) new-node-prob)
                             (do "making random node! "
                                 {:program (make-random-cat (count test-data))})
                             (let [creature1 (select-best scored-creatures tournament-size)
                                   creature2 (select-best scored-creatures tournament-size)]
                               (mutate (crossover creature1 creature2)))))]
       (println :elites (map :score elites))
       (recur (dec n) (into new-creatures elites))))))


(comment

  (def result (evolve 100 10 7 ["hi" true 5 8 7]))
  (first result)
  ;=> {:program (clojure.spec/cat :0 (s/* string?) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}

  (filter #(= 100 (:score %)) result)

 ;; ({:program (clojure.spec/cat :0 (s/* string?) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ string?))) :1 boolean? :2 (s/+ (s/+ (s/+ integer?))) :3 integer?), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* (s/+ (s/+ string?))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* string?) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* (s/+ (s/+ (s/+ (s/+ string?))))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ string?) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ string?)))))))) :1 (s/+ boolean?) :2 (s/* integer?)),
 ;;  :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ (s/+ string?)))))))) :1 (s/+ boolean?) :2 (s/* integer?)),
 ;;  :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ (s/+ (s/+ string?))))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ string?))) :1 boolean? :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ string?))) :1 (s/+ (s/+ boolean?)) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* (s/+ (s/+ (s/+ (s/+ string?))))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 string? :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* (s/+ string?)) :1 boolean? :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ string?)) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program
 ;;  (clojure.spec/cat :0 (s/+ (s/+ (s/+ string?))) :1 boolean? :2 (s/+ (s/+ integer?)) :3 (s/* (s/and (s/* string?) odd?))),
 ;;  :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ string?)) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ (s/+ string?)))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ string?))) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/* (s/+ string?)) :1 (s/* string?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ string?) :1 (s/+ boolean?) :2 (s/* integer?)), :score 100}
 ;; {:program (clojure.spec/cat :0 (s/+ (s/+ (s/+ (s/+ (s/+ string?))))) :1 boolean? :2 (s/* integer?)), :score 100})

)








