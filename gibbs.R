#---------------------------------gibbs-sampling--------------------------------
library(MASS)
library(MCMCpack)
library(mvtnorm)


bayesian_linear_model <- function(y,X,itteration = 10000) {

  y<-as.matrix(y)
  
  n <- dim(X)[1]
  x0<-rep(1,n)
  X <-cbind(x0,X)
  X <- as.matrix(X)
  
  p <- dim(X)[2]
  
  M0 <- rep(0,times=p)
  T0 <- diag(1000,p)
  e0=f0=0.5
  
  XTX <- as.matrix(crossprod(X))
  invT0 <- solve(T0)
  T0M0 <- invT0%*%M0
  tX <- t(X)
  
  Betas <- matrix(0,nrow=itteration,ncol=p)
  sigma2e <- rep(0,times=itteration)
  sigma2e[1] = .8
  
  
  for(i in 2:itteration) {
    VV <- as.matrix(XTX + invT0)
    vBetas <- solve(VV)
    
    mBetas <- vBetas%*%(tX%*%y + T0M0)
    
    betas <- mvrnorm(1,mBetas,vBetas*sigma2e[i-1])
    Betas[i,] <- betas
    
    Xb <- X%*%betas
    a0 <- n/2 + e0 #ee = a0
    e <- y-Xb
    b0 <- as.numeric(0.5*(t(e)%*%e)) + f0
    sigma2e[i] <- rinvgamma(1,shape=a0, scale=b0)
  }
  
  results <- NULL
  results$Mbetas <- apply(Betas[5000:itteration,],2,mean)
  results$SDbetas <- apply(Betas[5000:itteration,],2,sd)
  results$Msigma2e <- mean(sigma2e[5000:itteration])
  return(results)
}


## -----------------------------------usage-------------------------------------


X1 <- rnorm(500, 0, sd = 2)
X2 <- rnorm(500, 0, sd = 1)
X3 <- rnorm(500, 0, sd = 3)

y <- 2 + .5 * X1 - .8 * X2 +  0.2 * X3 + rnorm(500, 0, 1)

X <- cbind(X1, X2, X3)

bayesian_linear_model(y, X)
