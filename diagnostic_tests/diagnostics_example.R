#This script gives an example of why you need to use a diagnostic like the Stein discrepancy rather than traditional MCMC diagnostics when tuning the step size of the SGLD algorithm
#                                        
# For simplicity let's use the same example as given in the "Gaussian example"
#
# \pi(\theta) ~ N(0,P^T*D*P), where P is a 2x2 rotation matrix and D is a diagonal matrix                                                                               
#########################################################################################
# Set-up
source("ksd.R")
library(coda)

d = 2               #dimension
P = matrix(c(cos(pi/4),sin(pi/4),-sin(pi/4),cos(pi/4)),2,2) # rotation matrix
D = diag(c(2,1),d)                                          # diagonal matrix
Sigma = t(P)%*%D%*%P
hs = c(10^(-4),10^(-3),10^(-2),10^(-1),10^0)            #range of stepsizes
nu = 0.1           #standard deviation on noisy gradient

#SGLD sampler
reps = 10
iter = 10000
ess = matrix(NA,ncol=reps,nrow=length(hs)) # matrix to store effecive sample size
ksd = matrix(NA,ncol=reps,nrow=length(hs)) # matrix to store kernel Stein discrepancy
params = array(NA,dim=c(reps,iter,d))
matInv = -t(P)%*%solve(D)%*%P
for(k in 1:length(hs)){
    h = hs[k]
    for(j in 1:reps){
        theta = matrix(NA,nrow=iter,ncol=d)
        grads = matrix(NA,nrow=iter,ncol=d)
        theta[1,] = c(0,0)
        grads[1,] = matInv%*%theta[1,] + rnorm(2,0,nu)
        for(i in 2:iter){
            grads[i,] = matInv%*%theta[i-1,] + rnorm(2,0,nu)
            theta[i,] = theta[i-1,] + h/2*grads[i,] + sqrt(h)*rnorm(2,0,1)
        }
        params[k,,] = theta
        ess[k,j] = min(effectiveSize(theta))
        ksd[k,j] = log10(imqKSD(theta,grads,c=1,beta=0.5))
    }
}

#Plot posterior samples
library(ggplot2)
library(MASS)
library(gridExtra)
posterior = mvrnorm(100000,c(0,0),t(P)%*%solve(D)%*%P)
posterior=data.frame("Component 1"=posterior[,1],"Component 2"=posterior[,2])

p1 <- ggplot(posterior, aes(Component.1,Component.2)) +
    geom_density_2d() + labs(x=bquote(theta[1]),y=bquote(theta[2]))+
    geom_point(data=data.frame(x=params[1,seq(1,10000,10),1],y=params[1,seq(1,10000,10),2]),aes(x=x,y=y))


p2 <- ggplot(posterior, aes(Component.1,Component.2)) +
    geom_density_2d() + labs(x=bquote(theta[1]),y=bquote(theta[2]))+ ggtitle(bquote('h ='~10^-3)) + theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data=data.frame(x=params[2,seq(1,10000,10),1],y=params[2,seq(1,10000,10),2]),aes(x=x,y=y))
p3 <- ggplot(posterior, aes(Component.1,Component.2)) +
    geom_density_2d() + labs(x=bquote(theta[1]),y=bquote(theta[2]))+ ggtitle(bquote('h ='~10^-2)) + theme(plot.title = element_text(hjust = 0.5)) + 
    geom_point(data=data.frame(x=params[3,seq(1,10000,10),1],y=params[3,seq(1,10000,10),2]),aes(x=x,y=y))
p4 <- ggplot(posterior, aes(Component.1,Component.2)) +
    geom_density_2d() + labs(x=bquote(theta[1]),y=bquote(theta[2]))+ ggtitle(bquote('h ='~10^-1)) + theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data=data.frame(x=params[4,seq(1,10000,10),1],y=params[4,seq(1,10000,10),2]),aes(x=x,y=y))
p5 <- ggplot(posterior, aes(Component.1,Component.2)) +
    geom_density_2d() + labs(x=bquote(theta[1]),y=bquote(theta[2]))+ ggtitle(bquote('h ='~10^-0)) + theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data=data.frame(x=params[5,seq(1,10000,10),1],y=params[5,seq(1,10000,10),2]),aes(x=x,y=y))

p6 <- ggplot(data=data.frame(h=c(1,2,3,4),KSD=rowMeans(ksd[-1,])),aes(x=h,y=KSD)) +
                                        #  scale_x_discrete(limits=c("10e-3","10e-2","10e-1","10e0")) +
#    scale_x_discrete(limits=c(expression(paste("10"^"-3")),expression(paste("10"^"-2")),expression(paste("10"^"-1")),expression(paste("10"^"-0")))) +
     #     scale_x_discrete(limits=parse(text=expression(paste("10"^"-3")),expression(paste("10"^"-2")),expression(paste("10"^"-1")),expression(paste("10"^"-0")))) +
       geom_line()+
       geom_point()

p7 <- ggplot(data=data.frame(h=c(1,2,3,4),ESS=rowMeans(ess[-1,])),aes(x=h,y=ESS)) +
       scale_x_discrete(limits=c("10e-3","10e-2","10e-1","10e0")) +
       geom_line()+
       geom_point()

## lay <- rbind(c(1,2,3),
##              c(1,2,3),
##              c(4,5,5),
##              c(4,6,6))

lay <- rbind(c(1,2,3,4),
             c(1,2,3,4),
             c(5,5,6,6))

pdf("ksd.pdf", onefile = TRUE)             
grid.arrange(p2,p3,p4,p5,p6,p7, layout_matrix = lay)
dev.off()





