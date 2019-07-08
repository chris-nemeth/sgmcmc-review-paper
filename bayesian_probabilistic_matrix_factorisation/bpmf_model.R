library(sgmcmc)
library(mvtnorm)

#Log-likelihood function
logLik = function(params, data) {
    # Declare hyperparameter value alpha
    alpha = 1
    # Get user and item IDs corresponding to each rating (minus 1 because tensorflow indexes from 0)
    UIDs = tf$to_int32(data$user - 1)
    IIDs = tf$to_int32(data$item - 1)
    # Get entries of U and V matrices corresponding to users and items in current subsample
    UCurr = tf$gather(params$U, UIDs)
    VCurr = tf$gather(params$V, IIDs)
    # Calculate U^TV at required points
    UTV = tf$einsum('ij,ij->i', UCurr, VCurr)
    ll = tf$reduce_sum(tf$distributions$Normal(UTV, 1 / alpha)$log_prob(data$rating))
    # Add contribution of mu and Lambda only to current users 
    UDistn = tf$distributions$MultivariateNormalDiag(params$mu_U, 1 / tf$exp(params$Lambda_U))
    VDistn = tf$distributions$MultivariateNormalDiag(params$mu_V, 1 / tf$exp(params$Lambda_V))
    # Get all (unique) UIDs and IIDs currently being updated
    UUnique = tf$gather(params$U, tf$unique(UIDs)$y)
    VUnique = tf$gather(params$V, tf$unique(IIDs)$y)
    ll = ll + tf$reduce_sum(UDistn$log_prob(UUnique))
    ll = ll + tf$reduce_sum(VDistn$log_prob(VUnique))
    return(ll)
}

#Log-prior function
logPrior = function(params) {
    muUDistn = tf$distributions$MultivariateNormalDiag(scale_diag = 1 / tf$exp(params$Lambda_U))
    muVDistn = tf$distributions$MultivariateNormalDiag(scale_diag = 1 / tf$exp(params$Lambda_V))
    lp = muUDistn$log_prob(params$mu_U) + muVDistn$log_prob(params$mu_V) 
    LULogDens = tf$reduce_sum(3 * params$Lambda_U - 5 * tf$exp(params$Lambda_U))
    LVLogDens = tf$reduce_sum(3 * params$Lambda_V - 5 * tf$exp(params$Lambda_V))
    lp = lp + LULogDens + LVLogDens
    return(lp)
}

