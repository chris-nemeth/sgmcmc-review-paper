#This file runs each of the SGMCMC algorithms from the paper on a Bayesian neural network model

library(sgmcmc)
source("bnn_model.R")
source("bnn_setup.R")

#============================================================
# Get the dataset, seed and dimension info

iterations = 10^5
minibatchSize = 0.01

#Setup a data placeholder
testPlaceholder = list()
testPlaceholder[["X"]] = tf$placeholder(tf$float32, dim(testset[["X"]]))
testPlaceholder[["y"]] = tf$placeholder(tf$float32, dim(testset[["y"]]))

# Fill a feed dict with full test set (used to calculate log-loss)
feedDict = dict()
feedDict[[testPlaceholder[["X"]]]] = testset[["X"]]
feedDict[[testPlaceholder[["y"]]]] = testset[["y"]]

#---------------------------------------------------------------
#SGLD
h = 1e-4 #select a step size (KSD is recommended to tune the step size)

sgld = sgldSetup(logLik, dataset, params, h, logPrior = logPrior, minibatchSize)
# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = - logLik(sgld$params, testPlaceholder) / Ntest # log-loss function
sess = initSess(sgld) #Initialise the tensorflow session

sgld_logloss = vector(length=iterations) #track the log-los

# As the parameter space is high dimensional it will be difficult to store the full MCMC chain. We therefore use the sgmcmcStep function to iterate the algorithm without storing the full chain.

# Run chain
message("Running SGLDCV...")
for (i in 1:iterations){
    sgmcmcStep(sgld, sess)
    sgld_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sgld_logloss[i]))
    }
}

#------------------------------------------------------------
#SGLDCV
h = 1e-4
hOpt = 4e-5

sgldcv = sgldcvSetup(logLik, dataset, params, h, hOpt, logPrior = logPrior, minibatchSize)

# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = -logLik(sgldcv$params, testPlaceholder) / Ntest # log-loss function
sess = initSess(sgldcv) #Initialise the tensorflow session

sgldcv_logloss = vector(length=iterations) #track the log-los
# Run chain
message("Running SGLD...")
for (i in 1:iterations){
    sgmcmcStep(sgldcv, sess)
    sgldcv_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sgldcv_logloss[i]))
    }
}
#---------------------------------------------------------------
#SGHMC
h = 4e-6

sghmc = sghmcSetup(logLik, dataset, params, h, logPrior = logPrior, minibatchSize)
# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = -logLik(sghmc$params, testPlaceholder) / Ntest # log-loss function
sess = initSess(sghmc) #Initialise the tensorflow session

sghmc_logloss = vector(length=iterations) #track the log-los
# Run chain
message("Running SGHMC...")
for (i in 1:iterations){
    sgmcmcStep(sghmc, sess)
    sghmc_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sghmc_logloss[i]))
    }
}
#---------------------------------------------------------------
#SGHMCCV
h = 4e-6
hOpt = 1e-5

sghmccv = sghmccvSetup(logLik, dataset, params, h, hOpt, logPrior = logPrior, minibatchSize)
# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = -logLik(sghmccv$params, testPlaceholder) / Ntest # log-loss function

sess = initSess(sghmccv) #Initialise the tensorflow session

sghmccv_logloss = vector(length=iterations) #track the log-los
# Run chain
message("Running SGHMCCV...")
for (i in 1:iterations){
    sgmcmcStep(sghmccv, sess)
    sghmccv_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sghmccv_logloss[i]))
    }
}

#---------------------------------------------------------------
#SGNHT
h = 7e-6

sgnht = sgnhtSetup(logLik, dataset, params, h, logPrior = logPrior, minibatchSize)
# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = -logLik(sgnht$params, testPlaceholder) / Ntest # log-loss function
sess = initSess(sgnht) #Initialise the tensorflow session

sgnht_logloss = vector(length=iterations) #track the log-los
# Run chain
message("Running SGNHT...")
for (i in 1:iterations){
    sgmcmcStep(sgnht, sess)
    sgnht_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sgnht_logloss[i]))
    }
}

#---------------------------------------------------------------
#SGNHTCV
h = 7e-6
hOpt = 1e-5

sgnhtcv = sgnhtcvSetup(logLik, dataset, params, h, hOpt, logPrior = logPrior, minibatchSize)

# Get number of observations in test set
Ntest = as.double(nrow(testset[["X"]]))
logLoss = -logLik(sgnhtcv$params, testPlaceholder) / Ntest # log-loss function
sess = initSess(sgnhtcv) #Initialise the tensorflow session

sgnhtcv_logloss = vector(length=iterations) #track the log-los
# Run chain
message("Running SGNHTCV...")
for (i in 1:iterations){
    sgmcmcStep(sgnhtcv, sess)
    sgnhtcv_logloss[i] = sess$run(logLoss, feed_dict = feedDict)
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sgnhtcv_logloss[i]))
    }
}

#----------------------------------------------------------------
#Plots

plot(sgld_logloss,type='l',ylim=c(0,1))
points(sgldcv_logloss,type='l',col='red')
points(sghmc_logloss,type='l',col='blue')
points(sghmccv_logloss,type='l',col='green')
points(sgnht_logloss,type='l',col='orange')
points(sgnhtcv_logloss,type='l',col='purple')
