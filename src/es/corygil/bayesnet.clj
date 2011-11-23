(ns es.corygil.bayesnet
  "Belief propagation on trees."
  (:require
   [loom.graph :as graph])
  (:use incanter.core
        clojure.set))

;;FIX: polytrees

(defprotocol CPT
  (marginalize [this evidence])
  (transpose [this]))

(defn str-join [separator coll]
  (apply str
         (interpose separator coll)))

(deftype ConditionalProbabilityTable [cpt]
  CPT
  (marginalize [this evidence]
    (mmult cpt evidence))
  (transpose [this]
    (ConditionalProbabilityTable. (trans cpt)))
  Object
  (toString [this]
    (str cpt)))

(defrecord Node [id prior state]
  Object
  (toString [this] (name (:id this))))

(defn outgoing [g n]
  ;;FIX: assumes no cycles directly between two nodes. also this
  ;;should be a patch to loom.
  (difference (set (graph/neighbors g n))
              (set (graph/incoming g n))))


;;FIX: currently assumes all the nodes are connected. at least issue a
;;warning if not

(defn bayes-net [priors edges]
  (let [node-ids (set (flatten (keys edges)))
        nodes (into {}
                    (for [id node-ids]
                      [id (Node. id (priors id) (atom nil))]))]
    (apply graph/weighted-digraph ;;(ab)using the weight field of the
           ;;weighted graph to hold the CPT. some loom algs will
           ;;choke.
           (mapcat (fn [[[child p] cpt]]
                     (let [parents (if-not (coll? p)
                                     (vector p) p)]
                       (println parents)
                         (for [parent parents]
                           [(nodes parent) (nodes child)
                            (ConditionalProbabilityTable.
                             (matrix (for [row cpt]
                                       (if (seq? row)
                                         row
                                         [row (- 1 row)]))))]))) edges))))

(defmacro defbayes [name priors edges]
  `(def ~name
     (bayes-net ~priors ~edges)))

(defn terminal? [g node]
  (empty? (outgoing g node)))

(defn normalize [v]
  (div v (sum v)))

(defn sum-product [marginalized-probs]
  (if-let [ps (seq (filter identity marginalized-probs))]
    (do
      (println ps)
      (normalize
       (apply mult ps)))))

(defn- propagate-up [g parent evidence]
  (doseq [child (outgoing g parent)]
    (propagate-up g child evidence))
  (if-not @(.state parent)
    (reset! (.state parent)
            (if-let [evi (evidence (.id parent))]
              evi
              (normalize
               ((if (.prior parent)
                  (partial mult (.prior parent))
                  identity)
                (sum-product
                 (for [child (outgoing g parent)
                       :let [cpt (graph/weight g parent child)]
                       :when @(.state child)]
                   (marginalize cpt @(.state child))))))))))

(comment
  (let [marginalized
        (for [child (outgoing g parent)
              :let [cpt (graph/weight g parent child)
                    evi (evidence (:id child))]
              :when evi]
          (marginalize cpt evi))]
    (if (seq marginalized)
      (normalize
       ((if (.prior parent)
          (partial mult (.prior parent))
          identity)
        (sum-product marginalized))))))

(defn- propagate-down [g parent]
  (doseq [child (outgoing g parent)
          :let [cpt (graph/weight g parent child)]]
    (if-not @(.state child)
      (reset! (.state child)
              (marginalize (transpose cpt) ;;FIX: cpt should belong to node
                           (apply concat
                            (map #(deref (.state %)) (graph/incoming g child))))))
    (propagate-down g child)))

(comment
  (marginalize (transpose cpt) @(.state parent)))

(defn children [g node direction]
  (case direction
    :down (outgoing g node)
    :up (graph/incoming g node)))


(comment
  (defn propagate [g node cpts evidence state & {:keys [:direction]}]
   (let [child-fn (if (= direction :down)
                    outgoing
                    graph/incoming)
         state (if (= direction :down)
                 (merge state )
                 (propagate g ))]
     )
   (doseq [daughter ( g node)]
     (assoc state node
            (if (evidence node)
              (evidence node)
              (let [marginalized (for [child ])]))))))


(defn- results [g parent]
  (let [result (concat [[(:id parent) @(.state parent)]]
                       (mapcat (partial results g) (outgoing g parent)))]
    (reset! (.state parent) nil)
    result))

(defn roots [g]
  (filter #(empty? (graph/incoming g %)) (graph/nodes g)))

(defn infer [g evidence]
  (doseq [root (roots g)]
    (propagate-up g root evidence))
  (doseq [root (roots g)]
    (propagate-down g root))
  (reduce merge {}
          (map #(into {} (results g %))
               (roots g))))



;;;example

(defbayes disaster
  {:burglary [0.001 0.999]
   :earthquake [0.002 0.998]}
  {[:alarm [:burglary :earthquake]] [0.95 0.95 0.29 0.001]
   [:john-calls :alarm] [0.9 0.05]
   [:mary-calls :alarm] [0.7 0.01]})

(defbayes cancer
  {:cancer [0.01 0.04 0.95]}
  {[:t1 :cancer] [0.9 0.8 0.2]
   [:t2 :cancer] [0.9 0.8 0.2] 
   [:freaked-out :t1] [0.75 0.05]})

(defn node-by-id [g id]
  (first (filter #(= (:id %) id) (graph/nodes g))))


(def root
  (node-by-id cancer :cancer))