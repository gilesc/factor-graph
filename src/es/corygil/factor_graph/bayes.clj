(ns es.corygil.factor-graph.bayes
  (:use
   es.corygil.factor-graph)
  (:import
   [es.corygil.factor_graph FNode VNode]))

 ;;TODO: exception if invalid probability distribution :
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
