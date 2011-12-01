(ns es.corygil.factor-graph
  "Non-loopy belief propagation over factor graphs."
  (:import
   clojure.lang.Keyword))

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
(defmulti propagate (fn [g evidence messages [n1 n2]]
                      [(type (g n1)) (type (g n2))]))

(defn external? [node]
  (= (count (neighbors node)) 1))

(defmethod propagate [VNode FNode] [g evidence messages [v f]]
  "Propagate a variable state to a factor node by multiplying all the factor entries by the appropriate variable state entry. Returns an updated graph."
  (assoc messages [v f]
         (let [vnode (g v)]
           (if (external? vnode) ;;FIX: eliminate builtinstate
             (map (vec (:state vnode))
                  (factor-to-state-index g f v))
             (if-let [e (evidence v)]
               (map (vec e) (factor-to-state-index g f v))
               (reduce (partial map *)
                     (for [neighbor (neighbors vnode)
                           :when (not= neighbor f)]
                       (map 
                        (vec (messages [neighbor v]))
                        (factor-to-state-index g f v)))))))))

(defmethod propagate [FNode VNode] [g evidence messages [f v]]
  "Propagate a factor to a variable by summing the factor over all the possible states of the variable. Returns an updated graph."
  (assoc messages [f v]
         (let [fnode (g f)]
           (if (external? fnode)
             (:factor fnode)
             (let [fstate
                   (vec
                    (map *
                         (:factor fnode)
                         (reduce (partial map *)
                            (for [neighbor (neighbors fnode)
                                  :when (not= neighbor v)]
                              (messages [neighbor f])))))]
               (for [[state fidxs] (sort-by first (state-to-factor-index g v f))]
                 (reduce + (map fstate fidxs))))))))

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

(defn variable-states [g evidence messages]
  (into {}
   (for [[id node] g
         :when (= (type node) VNode)]
     [id
      (if-let [e (evidence id)]
        e
        (normalize
         (reduce (partial map *)
                 (for [f (neighbors node)]
                   (messages [f id])))))])))

(defn run-propagation
  "Run (nonloopy) belief propagation, starting at the given root node,
which should have only 1 neighbor."
  ([g root evidence]
     (assert (= (count (neighbors (g root))) 1)
             "Given node is not a suitable root node; it has multiple neighbors.")
     (let [schedule (update-schedule g root #{})
           messages
           (reduce (partial propagate (add-evidence g evidence) evidence)
                   {} (concat (reverse (map reverse schedule)) schedule))]
       (variable-states g evidence messages)))
  ([g evidence] (run-propagation g (find-root-node g) evidence))  ;;TODO: loopy
  ([g] (run-propagation g {})))


