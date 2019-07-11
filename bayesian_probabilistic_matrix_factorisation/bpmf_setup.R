library("sgmcmc")

# Generate dataset for STAN
genDataset = function() {
    # Generate dataset and set hyperparameters
    train <- read.table(paste0("data/train.dat"))
    test <- read.table(paste0("data/test.dat"))
    out = list()
    out$uid = train[,1]
    out$iid = train[,2]
    out$R = train[,3]
    out$meanRating = mean(out$R)
    out$uidTest = test[,1]
    out$iidTest = test[,2]
    out$T = test[,3]
    out$N = nrow(train)
    out$M = nrow(test)
    out$D = 20
    out$N_u = max(train[,1])
    out$N_i = max(train[,2])
    out$tau = 3
    out$mu0 = 0
    out$a0 = 1
    out$b0 = 5
    return(out)
}

# Generate dataset for SGMCMC
genSGDataset = function() {
    dataset = genDataset()
    # Delete unrequired items
    dataset = list("user" = dataset$uid, "item" = dataset$iid, "rating" = dataset$R)
    return(dataset)
}

# Generate the dataset f
genTestDataset = function() {
    dataset = genDataset()
    test <- read.table(paste0("data/test.dat"))
    # Delete unrequired items
    dataset = list("user" = test[,1], "item" = test[,2], "rating" = test[,3])
    return(dataset)
}


# Generate initial parameters for SGMCMC
initParams = function() {
    hyperParams = genDataset()
    # Initialize hyperparameters: mu and Lambda
    params = list("mu_U" = rnorm(hyperParams$D), "mu_V" = rnorm(hyperParams$D), 
                  "Lambda_U" = rnorm(hyperParams$D, -.1, .1), 
                  "Lambda_V" = rnorm(hyperParams$D, -.1, .1))
    # Sample main parameters U, V from prior
    params$U = rmvnorm(hyperParams$N_u, params$mu_U, diag(1 / exp(params$Lambda_U)))
    params$V = rmvnorm(hyperParams$N_i, params$mu_V, diag(1 / exp(params$Lambda_V)))
    return(params)
}



