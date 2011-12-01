(ns es.corygil.test.factor-graph
  (:use es.corygil.factor-graph
        es.corygil.factor-graph.bayes)
  (:use [clojure.test]))

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

(defn within [x y distance]
  (< (Math/abs (- x y))
     distance))

(deftest simple
  (is true
      (within (first
               (:alarm
                (run-propagation disaster {:john-calls [1 0]})))
              0.0434
              0.0001)))
