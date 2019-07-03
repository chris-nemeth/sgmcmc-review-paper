source("logistic_regression_setup.R")
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 1)

d = 10      # Choose a dimension for the parameters (10,50,100 in the paper)
N = 10^5    # number of data points

data = lookupData(d, N)

# Get prior variance for beta parameters
betaVar = betaPrior(d)

dataset = list("N" = N, "X" = data$X, "y" = data$y, "d" = data$d, "Sigma0" = betaVar)

stan_output = stan("logistic_regression_model.stan", data = dataset, iter = 2000, chains = 1)
