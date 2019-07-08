#Log-likelihood function
logLik = function(params, dataset) {
    # Calculate estimated probabilities
    beta = tf$nn$softmax(tf$matmul(dataset$X, params$B) + params$b)
    beta = tf$nn$softmax(tf$matmul(beta, params$A) + params$a)
    # Calculate log likelihood of categorical distn with probabilities beta
    logLik = tf$reduce_sum(dataset$y * tf$log(beta))
    return(logLik)
}

#Log-prior function
logPrior = function(params) {
    distLambda = tf$distributions$Gamma(1, 1)
    distA = tf$distributions$Normal(0, tf$rsqrt(params$lambdaA))
    logPriorA = tf$reduce_sum(distA$log_prob(params$A)) + distLambda$log_prob(params$lambdaA)
    distB = tf$distributions$Normal(0, tf$rsqrt(params$lambdaB))
    logPriorB = tf$reduce_sum(distB$log_prob(params$B)) + distLambda$log_prob(params$lambdaB)
    dista = tf$distributions$Normal(0, tf$rsqrt(params$lambdaa))
    logPriora = tf$reduce_sum(dista$log_prob(params$a)) + distLambda$log_prob(params$lambdaa)
    distb = tf$distributions$Normal(0, tf$rsqrt(params$lambdab))
    logPriorb = tf$reduce_sum(distb$log_prob(params$b)) + distLambda$log_prob(params$lambdab)
    logPrior = logPriorA + logPriorB + logPriora + logPriorb
    return(logPrior)
}
