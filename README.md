# This repository contains the R code from the paper *Stochastic gradient Markov chain Monte*

Running the code in this repository requires the `sgmcmc` [R package](https://arxiv.org/pdf/1710.00578.pdf).

Each of the models from the paper can be found in one of the following folders:
* Diagnostic tests (Section 4) - Code to compute the kernel Stein discrepancy.
* Logistic regression (Section 6.1) - Code to compare the *sgmcmc* algorithms on a logistic regression model using simulated data.
* Bayesian neural networks (Section 6.2) - Code to run the *sgmcmc* algorithms on a Bayesian neural network model using the popular MNIST dataset.
* Bayesian probabilistic matrix factorisation (Section 6.3) - Code to run the *sgmcmc* algorithms on the Bayesian probabilistic matrix factorisation model using the MovieLens dataset.

Note that within each folder for the SGMCMC simulations there is a file `run_algorithms.R` which is the main file to run each of the SGMCMC algorithms. The other files, e.g. `bpmf_setup.R` and `bpmf_model.R` provide utility functions and the Tensorflow model for the posterior targets, respectively. 
