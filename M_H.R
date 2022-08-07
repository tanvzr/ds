# Metropholis Hasting algorithm
library(MASS)
library(MCMCpack)
library(mvtnorm)

rm(list = ls())
# Sigma Known-------------------------------------------------------------------
##  likelihood------------------------------------------------------------------
Loglikelihood <- function(param, y, X) {
  
  X <- as.matrix(cbind(1,X))
  k <- ncol(X)
  beta <- param[1:k]
  sd <- param[k+1]
  yhat <- X %*% beta
  l <- dnorm(y, mean = yhat, sd = sd, log = T)
  logl <- sum(l)
  return(logl)
}


## prior------------------------------------------------------------------------
prior <- function(param) {
  a <- param[1]
  b1 <- param[2]
  b2 <- param[3]
  sd <- param[4]
  aprior <- dnorm(a, mean = 0, sd = 1000, log = T)
  b1prior <- dnorm(b1, mean = 0, sd = 1000, log = T)
  b2prior <- dnorm(b2, mean = 0, sd = 1000, log = T)
  sdprior <- dunif(sd, 0, 1000, log = T)
  return(aprior + b1prior + b2prior + sdprior)
}


## Posterior--------------------------------------------------------------------
posterior <- function(param, y, X) {
  return(Loglikelihood(param, y, X) + prior(param))
} 



## Proposal---------------------------------------------------------------------
proposalfunction <- function(param) {
  beta <- rnorm(3, mean = param[1:3], sd = c(0.1, 0.5, 0.3))
  sigma <- sig # since sigma known
  return(c(beta, sigma))
}


## Algorithm--------------------------------------------------------------------
metropolisMCMC_linear_Reg <- function(y, X, startvalue, iteration = 10000)
{
  Mchainvalue <- matrix(data = 0, nrow = iteration, ncol = 4)
  Mchainvalue[1, 1:4] <- startvalue #initial value
  
  for (i in 2:iteration) {
    proposal <- proposalfunction(Mchainvalue[i - 1, 1:4])
    probab <- exp(posterior(param = proposal, y, X) - posterior(param = Mchainvalue[i - 1, 1:4], y, X)) 
    #posterior dist jeta aga ber korsilam
    if (probab > runif(1)) {
      Mchainvalue[i, 1:4] <- proposal
    } else {
      Mchainvalue[i, 1:4] <- Mchainvalue[i - 1, 1:4]
    }
  }
  return(Mchainvalue)
}


## -----------------------------------------------------------------------------



sig <- 5 # sigma is known
X1 <- rnorm(500, 0, sd = 2)
X2 <- rnorm(500, 0, sd = 1)
y <- 4 + .8 * X1 - 1.1 * X2 + rnorm(500, 0, sig)
X <- cbind(X1, X2)


startvalue <- c(4, .2, 1, sig)
results <- metropolisMCMC_linear_Reg(y, X, startvalue)

apply(results[-c(1:2500), ],  2, FUN = mean)
summary(lm(y ~ X1 + X2))

# Plotting kore dekhi
plot(density(results[-c(1:500),1]))
plot(density(results[-c(1:500),2]))
plot(results[-c(1:500),2], type="l")




rm(list = ls())
# Sigma Unknown-----------------------------------------------------------------
##  likelihood------------------------------------------------------------------
Loglikelihood <- function(param, y, X) {
  
  X <- as.matrix(cbind(1,X))
  k <- ncol(X)
  beta <- param[1:k]
  sd <- param[k+1]
  yhat <- X %*% beta
  l <- dnorm(y, mean = yhat, sd = sd, log = T)
  logl <- sum(l)
  return(logl)
}


## prior------------------------------------------------------------------------
prior <- function(param) {
  a <- param[1]
  b1 <- param[2]
  b2 <- param[3]
  sd <- param[4]
  aprior <- dnorm(a, mean = 0, sd = 1000, log = T)
  b1prior <- dnorm(b1, mean = 0, sd = 1000, log = T)
  b2prior <- dnorm(b2, mean = 0, sd = 1000, log = T)
  sdprior <- dunif(sd, 0, 1000, log = T)
  return(aprior + b1prior + b2prior + sdprior)
}


## Posterior--------------------------------------------------------------------
posterior <- function(param, y, X) {
  return(Loglikelihood(param, y, X) + prior(param))
} 


## ----------------------------------------------------------------------------------------------
proposalfunction <- function(param) {
  betasig <- rnorm(4, mean = param[1:4], sd = c(0.1, 0.5, 0.3, .2))
  return(betasig)
}



## Algorithm--------------------------------------------------------------------
metropolisMCMC_linear_Reg <- function(y, X, startvalue, iteration = 10000)
{
  Mchainvalue <- matrix(data = 0, nrow = iteration, ncol = 4)
  Mchainvalue[1, 1:4] <- startvalue #initial value
  
  for (i in 2:iteration) {
    proposal <- proposalfunction(Mchainvalue[i - 1, 1:4])
    if (proposal[4] <= 0) {
      proposal <- proposalfunction(Mchainvalue[i - 1, 1:4])
    }
    probab <- exp(posterior(param = proposal, y, X) - posterior(param = Mchainvalue[i - 1, 1:4], y, X)) 
    if (probab > runif(1)) {
      Mchainvalue[i, 1:4] <- proposal
    } else {
      Mchainvalue[i, 1:4] <- Mchainvalue[i - 1, 1:4]
    }
  }
  return(Mchainvalue)
}



## -----------------------------------------------------------------------------



X1 <- rnorm(500, 0, sd = 2)
X2 <- rnorm(500, 0, sd = 1)
y <- 7 + .5 * X1 - .8 * X2 + rnorm(500, 0, 3)
startvalue <- c(4, .2, 1, sd(y))
X <- cbind(X1, X2)


results <- metropolisMCMC_linear_Reg(y, X, startvalue)

apply(results[-c(1:2500), ], 2, mean)
summary(lm(y ~ X1 + X2))

# Plotting kore dekhi
plot(density(results[-c(1:500),1]))
plot(density(results[-c(1:500),2]))
plot(results[-c(1:500),2], type="l")



## Mtcars Data -- Sigma Unknown (priors should be changed)

data("mtcars")   #Built in data
dat<-mtcars
y<-dat$mpg
X<-cbind(dat$hp, dat$wt)

startvalue <-  c(4,.2,1,1)
results <- metropolisMCMC_linear_Reg(y, X, startvalue)
apply(results[-c(1:2500),], 2, mean)

summary(lm(mpg~hp+wt, data = dat))


# Plotting kore dekhi
plot(density(results[-c(1:2500),1]))
plot(density(results[-c(1:2500),2]))
plot(results[-c(1:2500),2], type="l")

