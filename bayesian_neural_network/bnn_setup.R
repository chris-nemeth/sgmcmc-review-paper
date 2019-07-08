
# Download and load MNIST dataset
mnist = getDataset("mnist")
# Build dataset list and testset list
dataset = list("X" = mnist$train$images, "y" = mnist$train$labels)
testset = list("X" = mnist$test$images, "y" = mnist$test$labels)


# Sample initial weights from standard Normal
d = ncol(dataset$X) # dimension of chain
params = list()
params$A = matrix( rnorm(10*100), ncol = 10 )
params$B = matrix(rnorm(d*100), ncol = 100)
# Sample initial bias parameters from standard Normal
params$a = rnorm(10)
params$b = rnorm(100)
# Sample initial precision parameters from standard Gamma
params$lambdaA = rgamma(1, 1)
params$lambdaB = rgamma(1, 1)
params$lambdaa = rgamma(1, 1)
params$lambdab = rgamma(1, 1)


