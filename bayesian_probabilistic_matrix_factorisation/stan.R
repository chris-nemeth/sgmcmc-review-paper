library(rstan)
source("bpmf_setup.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dataset = genDataset()
meanRating = mean(dataset$R)
dataset$R = dataset$R - meanRating

output = stan("bpmf.stan", data = dataset, iter = 10000)
rmse = extract(output, "rmse")
