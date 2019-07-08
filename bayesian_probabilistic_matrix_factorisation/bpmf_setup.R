library("sgmcmc")

# Generate dataset for STAN
genDataset = function(inputs) {
    # Generate dataset and set hyperparameters
    train = inputs$train
    test = inputs$test
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
genSGDataset = function(inputs) {
    dataset = genDataset(inputs)
    # Delete unrequired items
    dataset = list("user" = dataset$uid, "item" = dataset$iid, "rating" = dataset$R)
    return(dataset)
}


# Generate initial parameters for SGMCMC
initParams = function(inputs) {
    hyperParams = genDataset(inputs)
    # Initialize hyperparameters: mu and Lambda
    params = list("mu_U" = rnorm(hyperParams$D), "mu_V" = rnorm(hyperParams$D), 
                  "Lambda_U" = rnorm(hyperParams$D, -.1, .1), 
                  "Lambda_V" = rnorm(hyperParams$D, -.1, .1))
    # Sample main parameters U, V from prior
    params$U = rmvnorm(hyperParams$N_u, params$mu_U, diag(1 / exp(params$Lambda_U)))
    params$V = rmvnorm(hyperParams$N_i, params$mu_V, diag(1 / exp(params$Lambda_V)))
    return(params)
}


# Download movielens dataset for bpmf example
# Dataset already broken into 5 seeds for cross validation, so break up accordingly
bpmfDownload = function(error = NULL) {
    writeLines("First time run. Downloading MovieLens dataset...")
    download.file("http://files.grouplens.org/datasets/movielens/ml-100k.zip", "tmp.zip")
    unzip("tmp.zip")
    # Remove last column and resave for each seed
    for (seed in 1:5) {
        dir.create(paste0(seed , "/"), showWarnings = FALSE)
        train = read.table(paste0("ml-100k/u", seed, ".base"))
        write.table(train[,1:3], paste0(seed, "/train.dat"), row.names = FALSE, col.names = FALSE)
        test = read.table(paste0("ml-100k/u", seed, ".test"))
        write.table(test[,1:3], paste0(seed, "/test.dat"), row.names = FALSE, col.names = FALSE)
    }
    unlink("ml-100k", recursive = TRUE)
    unlink("tmp.zip", recursive = TRUE)
}

# Load required data based on array ID
lookup = function(seed) {
    # Check seed is given
    if (length(seed) == 0) {
        stop("Please provide command line argument for seed")
    }
    out = list("seed" = seed)
    datasets = bpmf(out$seed)
    out = c(out, datasets)
    set.seed(out$seed)
    return(out)
}

# Try to open relevant bpmf data. If it fails then download it.
bpmf = function(seed) {
    train <- read.table(paste0("data/", seed, "/train.dat"))
    test <- read.table(paste0("data/", seed, "/test.dat"))
    return(list("train" = train, "test" = test))
}

