(ns es.corygil.factor-graph
  "Non-loopy belief propagation over factor graphs."
  (:import
   clojure.lang.Keyword))

"
Possible improvements:
  * Incorporate continuous variables
  * Loopy BP
  * make a n-dimensional matrix representation"

;;TODO: clean up all the coercions to vector and make some things lazy



(defprotocol NodeP
  (neighbors [this]))

(defrecord FNode [neighbors factor]
  NodeP
  (neighbors [this] neighbors))

(defrecord VNode [neighbors state]
  NodeP
  (neighbors [this] neighbors))

(defmulti dims
  "Return the dimensions of the given factor or variable node.
   For a variable node, this will be an integer (number of states the variable can assume).
   For a factor node, this will be a ordered vector of the dimensions of its (variable node) neighbors."
  (fn [g n]
      (type n)))

(defmethod dims Keyword [g n]
  (dims g (g n)))

(defmethod dims FNode [g n]
  (for [neighbor (neighbors n)]
    (dims g (g neighbor))))

(defmethod dims VNode [g n]
  (count (:state n)))


(defn cumulative-product [v]
  (reduce (fn [acc n]
            (conj acc (* (or (last acc) 1) n)))
          [] v))

(defn- index-vector [dims]
  (reverse
   (cons 1
         (cumulative-product
          (reverse (drop 1 dims))))))

(defn- index-of [v q]
  (ffirst
   (filter #(= (second %) q)
           (map-indexed vector v))))

(defn- factor-to-state-index [g f v]
  "Return a seq, the same length as the given factor node's factor vector, containing the state indices mapping the given variable node to the factor vector."
  (let [idx (index-of (neighbors (g f)) v)
        dims (vec (dims g f))
        idxv (vec (index-vector dims))
        result
        (reduce concat
            (map #(repeat (idxv idx) %)
                 (range (dims idx))))]
    (reduce concat
            (repeat (/ (reduce * dims)
                       (count result))
                    result))))

(defn- state-to-factor-index [g v f]
  "Return a map of state indices to factor indices. Basically an inversion of factor-to-state-index."
  (into {}
   (for [[state-idx factor-state-index-pairs]
         (group-by second
                   (map-indexed vector
                                (factor-to-state-index g f v)))]
     [state-idx (map first factor-state-index-pairs)])))

(defn update-schedule [graph id visited]
  "Return a seq of node pairs representing a breadth first traversal starting from the root node (id)."
  (when-let [children (remove visited (neighbors (graph id)))]
    (lazy-cat (map #(vector id %) children)
            (reduce concat
                    (map #(update-schedule graph %
                                           (conj visited id)) children)))))

(def propagate nil)
(defmulti propagate (fn [[_ g] [n1 n2]]
                      [(type (g n1)) (type (g n2))]))

(defmethod propagate [VNode FNode] [[evidence g] [v f]]
  "Propagate a variable state to a factor node by multiplying all the factor entries by the appropriate variable state entry. Returns an updated graph."
  (println v f (map (vec (:state (g v)))
                    (factor-to-state-index g f v))
           (map *
                  (:factor (g f))
                  (map (vec (:state (g v)))
                       (factor-to-state-index g f v))))
  [evidence
   (if (> (count (neighbors (g f))) 1)
        (assoc-in g [f :factor]
              (map *
                   (:factor (g f))
                   (map (vec (:state (g v)))
                        (factor-to-state-index g f v))))
        g)])


(defn state [g v]
  (for [f (neighbors (g v))]
    (for [[state-idx f-idxs] (sort-by first (state-to-factor-index g v f))]
          (reduce +
            (map (vec (:factor (g f))) f-idxs)))))

(defmethod propagate [FNode VNode] [[evidence g] [f v]]
  (println f v (evidence v)
           (for [[state-idx f-idxs]
                          (sort-by first (state-to-factor-index g v f))]
                      (reduce +
                              (map (vec (:factor (g f))) f-idxs))))
  "Propagate a factor to a variable by summing the factor over all the possible states of the variable. Returns an updated graph."
  [evidence
   (if-not (v evidence)
     (assoc-in g [v :state]
               (map *
                    (:state (g v))
                    (for [[state-idx f-idxs]
                          (sort-by first (state-to-factor-index g v f))]
                      (reduce +
                              (map (vec (:factor (g f))) f-idxs)))))
     g)])

(defn normalize [v]
  (let [sum (reduce + v)]
    (map #(/ % sum) v)))

(defn find-root-node [g]
  (first (for [[id node] g
               :when (and (= (type node) FNode)
                          (= (count (neighbors node)) 1))]
           id)))

(defn- add-evidence [graph evidence]
  (into {}
        (for [[id node] graph]
          [id
           (if-let [evi (id evidence)]
             (assoc node :state evi) node)])))

(defn run-propagation
  "Run (nonloopy) belief propagation, starting at the given root node, which should have only 1 neighbor."
  ([g root evidence]
     (assert (= (count (neighbors (g root))) 1)
             "Given node is not a suitable root node; it has multiple neighbors.")
     (let [schedule (update-schedule g root #{})]
       (second
        (reduce propagate [evidence (add-evidence g evidence)]
                (concat (reverse (map reverse schedule)) schedule)))))
  ([g evidence] (run-propagation g (find-root-node g) evidence))  ;;TODO: loopy
  ([g] (run-propagation g nil)))

(defn variable-states [g]
  (into {}
   (for [[id node] g
         :when (= (type node) VNode)
         :let [sum (reduce + (:state node))]]
     [id (map #(/ % sum) (:state node))])))

 ;;TODO: exception if invalid probability distribution :(
(defn bayes-net [variables factors]
  "Create a factor graph from a compact Bayes Net representation."
  (loop [nodes {} factors (concat factors (filter (comp coll? second) variables))]
    (if-not (seq factors)
      nodes
      (recur
       (merge nodes
              (let [[vs fs] (first factors)
                    vs (if (coll? vs)
                         (concat (drop 1 vs) [(first vs)])
                         [vs])
                    fnode (FNode. vs fs)
                    fkey (keyword (gensym "factor"))]
                (into {}
                      (concat [[fkey fnode]]
                              (for [v vs
                                    :let [node
                                          (or (nodes v)
                                              (VNode. []
                                                      (repeat (let [state (variables v)]
                                                                (if (coll? state)
                                                                  (count state)
                                                                  (int state)))
                                                              1)))]]
                                [v (assoc node
                                     :neighbors (conj (:neighbors node) fkey))])))))
       (rest factors)))))

(def disaster
  (bayes-net
   {:burglary [0.001 0.999]
    :earthquake [0.002 0.998]
    :alarm 2
    :john-calls 2
    :mary-calls 2}
   {[:alarm :burglary :earthquake] [0.95 0.05 0.94 0.06 0.29 0.71 0.001 0.999]
    [:john-calls :alarm] [0.9 0.1 0.05 0.95]
    [:mary-calls :alarm] [0.7 0.3 0.01 0.99]})) 
