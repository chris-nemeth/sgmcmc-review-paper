#This file runs each of the SGMCMC algorithms from the paper on a Bayesian probabilistic matrix factorisation model

library(sgmcmc)
source("bpmf_model.R")
source("bpmf_setup.R")

#============================================================
# Get the dataset and set-up some test functions
set.seed(13579)

dataset = genSGDataset()    # Load the trainin data
testData = genTestDataset() # Load the test data
params = initParams()       # Initial parameters for SGMCMC

#Centre the data
meanRating = mean(dataset$rating)
dataset$rating = dataset$rating - meanRating

#Predict movie rating on held out users
predict = function(currentState,testset,meanRating){
    pred_out = rowSums(currentState$U[testset[["user"]],]*currentState$V[testset[["item"]],]) + meanRating
    pred_out[pred_out>5] = 5
    pred_out[pred_out<1] = 1
    return(pred_out)
}

#Set-up test data set
testset = list("user" = testData$user, "item" = testData$item, "rating" = testData$rating)
testPlaceholder = list()
testPlaceholder[["user"]] = tf$placeholder(tf$float32, length(testset[["user"]]))
testPlaceholder[["item"]] = tf$placeholder(tf$float32, length(testset[["item"]]))
testPlaceholder[["rating"]] = tf$placeholder(tf$float32, length(testset[["rating"]]))

#Set up a feed dict to calculate the predictive accuracy
feedDict = dict()
feedDict[[testPlaceholder[["user"]]]] = testset[["user"]]
feedDict[[testPlaceholder[["item"]]]] = testset[["item"]]
feedDict[[testPlaceholder[["rating"]]]] = testset[["rating"]]

#--------------------------------------------------------------------------------
#Run the algorithms with setup

iter = 10^5         # Number of SGMCMC iterations
optIter = 10^4      # Number of optimisation steps for the CV methods
minibatchSize = 0.1 # Number of subsamples per iteration

#-------------------------------------------------------------------
# SGLD
h = 4e-5 

sgld = sgldSetup(logLik, dataset, params, h, logPrior, minibatchSize)

sess = initSess(sgld)  #Initialise the Tensorflow session
sgld_rmse = rep(0, iter)

for (i in 1:iter) {
    sgmcmcStep(sgld, sess)
    currentState = getParams(sgld, sess)
    pred = predict(currentState,testset,meanRating)
    sgld_rmse[i] = sqrt(mean((testset[["rating"]]-pred)^2))
        if (i %% 100 == 0) {
        message(paste0(i, "\t", sgld_rmse[i]))
    }
}

#------------------------------------------------
# SGLDCV
h = 1e-10
hOpt = 4e-5

sgldcv = sgldcvSetup(logLik, dataset, params, h, hOpt, logPrior, minibatchSize, nItersOpt=optIter)

sess = initSess(sgldcv)  #Initialise the Tensorflow session
sgldcv_rmse = rep(0, iter)

for (i in 1:iter) {
    sgmcmcStep(sgldcv, sess)
    currentState = getParams(sgldcv, sess)
    pred = predict(currentState,testset,meanRating)
    sgldcv_rmse[i] = sqrt(mean((testset[["rating"]]-pred)^2))
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sgldcv_rmse[i]))
    }
}

#------------------------------------------------------------------
# SGHMC
h = 4e-7 

sghmc = sghmcSetup(logLik, dataset, params, h, logPrior, minibatchSize)

sess = initSess(sghmc)  #Initialise the Tensorflow session
sghmc_rmse = rep(0, iter)

for (i in 1:iter) {
    sgmcmcStep(sghmc, sess)
    currentState = getParams(sghmc, sess)
    pred = predict(currentState,testset,meanRating)
    sghmc_rmse[i] = sqrt(mean((testset[["rating"]]-pred)^2))
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sghmc_rmse[i]))
    }
}

#-------------------------------------------------------------------
# SGHMCCV
h = 1e-12
hOpt = 4e-5

sghmccv = sghmccvSetup(logLik, dataset, params, h, hOpt, logPrior, minibatchSize, nItersOpt=optIter)

sess = initSess(sghmccv)  #Initialise the Tensorflow session
sghmccv_rmse = rep(0, iter)

for (i in 1:iter) {
    sgmcmcStep(sghmccv, sess)
    currentState = getParams(sghmccv, sess)
    pred = predict(currentState,testset,meanRating)
    sghmccv_rmse[i] = sqrt(mean((testset[["rating"]]-pred)^2))
    if (i %% 100 == 0) {
        message(paste0(i, "\t", sghmccv_rmse[i]))
    }
}

#------------------------------------------------------------------------
#Plot the rmse

plot(test$sgld_rmse,type='l',ylim=c(0.8,max(sgld_rmse)))
points(sgldcv_rmse,type='l',col='red')
points(sghmc_rmse,type='l',col='blue')
points(sghmccv_rmse,type='l',col='green')
