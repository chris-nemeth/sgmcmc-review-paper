library(sgmcmc)
source("logistic_regression_setup.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# Get the dataset, seed and dimension info

d = 10              # Choose a dimension for the parameters (10,50,100 in the paper)
N = 10^5            # Number of data points
iter = 10^5         # Number of SGMCMC iterations
optIter = 5*10^4    # Number of optimisation steps for the CV methods

data = lookupData(d, N)
minibatchSize = 0.01
dataset = list("X" = data$X, "y" = data$y)
params = list("beta" = matrix(betaInit(data$d),nrow=data$d))

#------------------------------------------------------------------
# SGLD
h = 4e-7      # step size parameter

sgld_output = sgld(logLik, dataset, params, h, logPrior, minibatchSize, nIters = iter)

#------------------------------------------------------------------
# SGLDCV
h = 4e-7      # step size parameter
hOpt = 1e-6   # optimisation step size parameter

sgldcv_output = sgldcv(logLik, dataset, params, h, hOpt, logPrior, minibatchSize, nIters = iter/2, nItersOpt = optIter)

#------------------------------------------------------------------
# SGHMC
h = 1e-8      # step size parameter

sghmc_output = sghmc(logLik, dataset, params, h, logPrior, minibatchSize, nIters = iter)

#------------------------------------------------------------------
# SGHMCCV
h = 1e-8      # step size parameter
hOpt = 4e-7   # optimisation step size parameter

sghmccv_output = sghmccv(logLik, dataset, params, h, hOpt, logPrior, minibatchSize, nIters = iter/2, nItersOpt = optIter)

#------------------------------------------------------------------
# SGNHT
h = 1e-8      # step size parameter

sgnht_output = sgnht(logLik, dataset, params, h, a=0.02, logPrior, minibatchSize, nIters = iter)

#------------------------------------------------------------------
# SGNHTCV
h = 1e-9      # step size parameter
hOpt = 4e-7   # optimisation step size parameter

sgnhtcv_output = sgnhtcv(logLik, dataset, params, h, hOpt, logPrior, minibatchSize, a=0.02, nIters = iter/2, nItersOpt = optIter)

#------------------------------------------------------------------
# ULA - SGLD with the full dataset
h = 4e-7      # step size parameter

ula_output = sgld(logLik, dataset, params, h, logPrior, dim(dataset$X)[1], nIters = iter)

#-----------------------------------------------------------------
#Plot the results

library(ggplot2)
library(reshape2)
library(gridExtra)


#SGLD
posterior <- data.frame("Iteration"=seq(1,nrow(sgld_output$beta)),"Samples"=sgld_output$beta)
posterior <- melt(posterior,id="Iteration")
p1 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() +  labs(x="Iteration",y=bquote(theta),title="SGLD") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#SGLDCV
posterior <- data.frame("Iteration"=seq(1,nrow(sgldcv_output$beta)),"Samples"=sgldcv_output$beta)
posterior <- melt(posterior,id="Iteration")
p2 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() +  labs(x="Iteration",y=bquote(theta),title="SGLDCV") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#ULA
posterior <- data.frame("Iteration"=seq(1,nrow(ula_output$beta)),"Samples"=ula_output$beta)
posterior <- melt(posterior,id="Iteration")
p3 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() +  labs(x="Iteration",y=bquote(theta),title="ULA") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#SGHMC
posterior <- data.frame("Iteration"=seq(1,nrow(sghmc_output$beta)),"Samples"=sghmc_output$beta)
posterior <- melt(posterior,id="Iteration")
p4 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() + labs(x="Iteration",y=bquote(theta),title="SGHMC") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#SGHMCCV
posterior <- data.frame("Iteration"=seq(1,nrow(sghmccv_output$beta)),"Samples"=sghmccv_output$beta)
posterior <- melt(posterior,id="Iteration")
p5 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line()  + labs(x="Iteration",y=bquote(theta),title="SGHMCCV") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#SGNHT
posterior <- data.frame("Iteration"=seq(1,nrow(sgnht_output$beta)),"Samples"=sgnht_output$beta)
posterior <- melt(posterior,id="Iteration")
p6 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() + labs(x="Iteration",y=bquote(theta),title="SGNHT") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8))

#SGNHTCV
posterior <- data.frame("Iteration"=seq(1,nrow(sgnhtcv_output$beta)),"Samples"=sgnhtcv_output$beta)
posterior <- melt(posterior,id="Iteration")
p7 <- ggplot(posterior, aes(x=Iteration,y=value,group=variable,color=variable)) + geom_line() + labs(x="Iteration",y=bquote(theta),title="SGNHTCV") + theme(legend.position = "none",plot.title = element_text(hjust=0.5,size = (10)),axis.text=element_text(size=5),axis.title=element_text(size=8)) 

lay <- rbind(c(1,2,3,4),
             c(5,6,7,8))
grid.arrange(p1,p2,p3,p4,p5,p6,p7, layout_matrix = lay)

