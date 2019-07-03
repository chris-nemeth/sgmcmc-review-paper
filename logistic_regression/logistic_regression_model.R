#Log-likelihood function
logLik = function(params, dataset) {
    logLik = tf$reduce_sum(dataset$y * tf$squeeze(tf$matmul(dataset$X, params$beta)) - tf$log(1 + tf$exp(tf$squeeze(tf$matmul(dataset$X, params$beta)))))
    return(logLik)
}

#Log-prior
logPrior = function(params) {
    d = tf$shape(params$beta)[1L]
    mu0 = tf$zeros(d)
    Sigma0 = 10 * tf$ones(d)
    priorDistn = tf$distributions$MultivariateNormalDiag(mu0, Sigma0)
    logPrior = tf$reduce_sum(priorDistn$log_prob(params$beta))
    return(logPrior)
}


