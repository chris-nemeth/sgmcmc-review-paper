library(mvtnorm)
source("logistic_regression_model.R")

# Create required dataset for current simulation
lregDim = function(d, N) {
    # Vary scales to simulate beta from so parameter contribution varies
    beta_scales = c(.1, 1, 10)
    # Generate covariance matrix for covariates Sigma0.
    # Of the form diag(Sigma0) = 1 and Sigma0[i,j] ~ Unif(-rho, rho)^(i-j)
    # The aim is to emulate standardised covariates with a small amount of correlation
    rho = .4
    Sigma0 = genCovMat(d, rho)
    # Simulate covariates, parameters and predictors
    X = rmvnorm(N, rep(0, d), Sigma0)
    beta = rnorm(d, 0, sample(beta_scales, d, replace = T))
    probs = apply(X, 1, function (x_i) 1 / (1 + exp(-sum(x_i * beta))))
    y = rbinom(length(probs), 1, probs)
    params = list("beta"=beta)
    data = list("X" = X, "y" = y)
    # Store additional constants
    out = list("data" = data, "params"= params)
}

# Generate a Symmetric, correlated matrix Sigma0 with 1s on diagonal
genCovMat = function(d, rho) {
    # Generate matrix with all 1 entries
    Sigma0 = matrix(rep(1, d^2), ncol = d)
    # Simulate lower triangular part from U(-rho, rho)^(distance from i)
    for (i in 2:d) {
        for (j in 1:(i-1)) {
            Sigma0[i,j] = runif(1, -rho, rho)^(i-j)
        }
    }
    # Make matrix symmetric
    Sigma0 = Sigma0 * t(Sigma0)
    return(Sigma0)
}


# Get some data
lookupData = function(d,N) {
    dataset = lregDim(d, N) 
    out = dataset$data
    out$N = nrow(out$X)
    out$d = ncol(out$X)
    return(out)
}

# Set prior variance for Beta
betaPrior = function(d) {
    return(diag(rep(10, d)))
}

# Sample initial beta values from random normal
betaInit = function(d) {
    return(rnorm(d))
}
