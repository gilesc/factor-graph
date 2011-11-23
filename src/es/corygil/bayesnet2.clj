(ns es.corygil.bayesnet2
  (:require
   [loom.graph :as graph])
  (:use incanter.core
        clojure.set))

(defrecord Node [state cpt parents children])

(defn- ensure-coll [x]
  (if-not (coll? x)
    [x] x))

(defn to-cpt [v]
  (for [row v]
    (if (seq? row)
      row
      [row (- 1 row)])))

(defn bayes-net [probabilities]
  (let [nodes (into {}
                    (for [[k p] probabilities]
                      [(first (ensure-coll k))
                       (Node.
                        nil
                        (matrix
                         (if-not (coll? k)
                           p (to-cpt p)))
                        (if (coll? k)
                          (ensure-coll
                           (second k))) nil)]))
        children (group-by first
                           (for [[id node] nodes
                                 parent (:parents node [])]
                             [parent id]))]
    (into {}
          (for [[id node] nodes]
            [id (assoc node :children (map second (children id)))]))))

(defn roots [net]
  (for [[id node] net
        :when (empty? (:parents node))]
    id))

(defn leaves [net]
  (for [[id node] net
        :when (empty? (:children node))]
    id))

(defn normalize [v]
  (div v (sum v)))

(defn sum-product [marginalized-probs]
  (if (seq marginalized-probs)
    (normalize
     (apply mult marginalized-probs))) )

(defn marginalized? [p]
  (= (ncol p) 1))

(defn propagate [net id evidence]
  (let [node (net id)
        net (reduce merge net
                    (map #(select-keys (propagate net % evidence) [%])
                         (:children node)))]
    (assoc-in net [id :state]
              (if (evidence id)
                (evidence id)
                ((if-not (:parents node)
                   (partial mult (:cpt node))
                   identity)
                 (sum-product
                  (for [child-id (:children node)
                        :let [child (net child-id)]
                        :when (:state child)]
                    (mmult (:cpt child) (:state child)))))))))



(comment
  )

(defn infer [net evidence])


(defmacro defbayes [name probs]
  `(def ~name
     (bayes-net ~probs)))

(defbayes cancer
  {:cancer [0.01 0.04 0.95]
   [:t1 :cancer] [0.9 0.8 0.2]
   [:t2 :cancer] [0.9 0.8 0.2]
   [:freaked-out :t1] [0.75 0.05]})

(defbayes disaster
  {:burglary [0.001 0.999]
   :earthquake [0.002 0.998]
   [:alarm [:burglary :earthquake]] [0.95 0.95 0.29 0.001]
   [:john-calls :alarm] [0.9 0.05]
   [:mary-calls :alarm] [0.7 0.01]})

(println "\n\n\n")
(pprint
 (propagate disaster :burglary {:john-calls [1 0] :mary-calls [0 1]}))

