# factor-graph

A factor graph is a generic representation of the factorization of a
function, and can be used to represent a wide variety of operations,
from Kalman filters to FFTs to undirected and directed probabilistic
graphical models like Bayesian nets, Markov models, and CRFs.

This package is currently designed mainly from the perspective of
probabilistic graphical models. For example, it can perform belief
propagation over a Bayesian net given some evidence. Right now, it
only supports discrete "functions", not continuous ones, and only does
non-loopy belief propagation.

## Usage

### Directed PGMs (Bayes nets)

First, create a factor graph using Bayesian net style notation:

	(use 'es.corygil.factor-graph)
	(use 'es.corygil.factor-graph.bayes)
	(def disaster
	  (bayes-net
	    {:burglary [0.001 0.999]
	     :earthquake [0.002 0.998]
	     :alarm 2
	     :john-calls 2
	     :mary-calls 2}
	    {[:alarm :burglary :earthquake] 
	     [0.95 0.05 0.94 0.06 0.29 0.71 0.001 0.999]
	     [:john-calls :alarm] [0.9 0.1 0.05 0.95]
	     [:mary-calls :alarm] [0.7 0.3 0.01 0.99]}))

The bayes-net function takes two arguments: the first is a map of all
the variable nodes in the network. If the node has no parents, then in
a Bayesian network, it requires a prior. So, in the above example, the
prior probability of a burglary is 0.001 and the probability of not
burglary is 0.999. If a node does have parents, then it is only
necessary to specify the number of states it can assume.

The second argument contains the conditional probability tables in a
flattened format. The first entry, for example, would be read as "the
probability of alarm given burglary and earthquake", where 0.95 is the
p(B,E,A) 0.05 is p(B,E,~A), 0.94 is p(B,~E,A), and so forth.

Now we can perform belief propagation:

	(run-propagation disaster)

will give us the marginals for all the variable nodes in the absence
of any evidence. Or we can use evidence:

	(run-propagation disaster {:john-calls [1 0]})

	> {:burglary (0.016283729946769937 0.98371627005323),
	   :john-calls [1 0],
	   :mary-calls (0.03997202114194967 0.9600279788580504),
	   :alarm (0.04343771179992705 0.9565622882000729),
	   :earthquake (0.011394968773811182 0.9886050312261888)}

### Undirected PGMs

TODO

## TODO

* Loopy belief propagation
* More generic factor graph constructors
* Allow factors to be arbitrary functions, not just vectors

## License

Copyright (C) 2011 Cory Giles

Distributed under the Eclipse Public License, the same as Clojure.
