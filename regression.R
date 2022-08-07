# LINEAR REGRESSION
# Simple linear regression
# estimating the parameters of this model using optim R-function

rm(list = ls())
library(broom)

set.seed(123)

x1 <- rnorm(100, 5, sd = 4)
y <- 10 + 2*x1 + rnorm(100, 0, 1)

ll_line <- function(y, x, par) {
  y = as.matrix(y)
  x = as.matrix(cbind(1, x))
  k = ncol(x)
  beta = par[1:k]
  se = exp(par[k+1])
  
  l = dnorm(x = y, mean = x %*% beta, sd = se, log = T)
  
  return(-sum(l))
}

# optim function
res <- optim(par = c(1, 2, 3), y = y, x = x1, fn = ll_line, hessian = T)
tidy(res) # std err of estimates

# the value of sigma
exp(res$par[3])

# fitting the linear model using "lm"R-function
model<- lm(y ~ x1)
summary(model)$coef

tidy(model) # for model summary
glance(model) # for goodness of fit
augment(model) # for fitted values and residuals

tidy(res) ; tidy(model)


# Multiple Linear Regression
# Task-01: X1 ~ N(25, sd = 4), X2 ~ Bernoulli(p = 0.5). Generate 100 obs for 
# Y = XB + error. Where error ~ N(0,1), Bo = 25, B1 = 1.5, B2 = 2. Assess how the
# R-function "optim"and "lm"produce unbias estimates for B's and their variances.

rm(list = ls())
library(broom)

set.seed(123)

x1 <- rnorm(100, 25, sd = 4)
x2 <- rbinom(n = 100, size = 1, prob = 0.5)
y <- 25 + 1.5 * x1 + 2 * x2 + rnorm(100)

ll_line <- function(y, x, par) {
  y = as.matrix(y)
  x = as.matrix(cbind(1, x))
  k = ncol(x)
  beta = par[1:k]
  se = exp(par[k+1])
  
  l = dnorm(x = y, mean = x %*% beta, sd = se, log = T)
  
  return(-sum(l))
}

# optim function
res <- optim(par = c(1, 2, 3, 4), y = y, x = cbind(x1, x2), fn = ll_line, hessian = T)
tidy(res) # std err of estimates

# the value of sigma
exp(res$par[4])

# fitting the linear model using "lm"R-function
model<- lm(y ~ x1+x2)
summary(model)$coef

tidy(model) # for model summary
glance(model) # for std error and goodness of fit
augment(model) # for fitted values and residuals

tidy(res) ; tidy(model)





# Logistic Regression
# Estimating the parameters of the logistic regression using Optim

rm(list = ls())
library(broom)

set.seed(123)

x1<- rnorm(1000, 0, sd = 1)

xb<- 2+3*x1
p<- 1/(1+exp(-xb))
y<- rbinom(1000, 1, p)

ll_logistic <- function(y, x, par) {
  y = as.matrix(y)
  x = as.matrix(cbind(1, x))
  k = ncol(x)
  beta = par[1:k]
  pix = exp(x %*% beta)/(1+exp(x %*% beta))
  
  l = dbinom(x = y, size = 1, prob = pix, log = T)
  
  return(-sum(l))
}

# optim function
res <- optim(par = c(1, 2), y = y, x = x1, fn = ll_logistic, hessian = T)
tidy(res) # std err of estimates

# fitting the linear model using "lm"R-function
model<- glm(formula = y ~ x1, family = binomial(link = "logit"))

tidy(model) # for model summary
glance(model) # for std error and goodness of fit
augment(model) # for fitted values and residuals

tidy(res) ; tidy(model)

# Example: GPA & GRE
rm(list = ls())
library(broom)
dat <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

y <- dat$admit
x1 <- cbind(dat$gre, dat$gpa)

ll_logistic <- function(y, x, par) {
  y = as.matrix(y)
  x = as.matrix(cbind(1, x))
  k = ncol(x)
  beta = par[1:k]
  pix = exp(x %*% beta)/(1+exp(x %*% beta))
  
  l = dbinom(x = y, size = 1, prob = pix, log = T)
  
  return(-sum(l))
}

# optim function
res <- optim(par = c(0,0,0), y = y, x = x1, fn = ll_logistic, hessian = T)
tidy(res) # std err of estimates

# By using glm function
model <- glm(formula = admit ~ gre + gpa, family = "binomial", data = dat)

tidy(model) # for model summary
glance(model) # for std error and goodness of fit
augment(model) # for fitted values and residuals

tidy(res) ; tidy(model)

# Newton-Raphson method
# By using Newton-Raphson 

logit_NR <- function(y, x){
  diff = 19
  y = as.matrix(y)
  x = cbind(1, x)
  k = ncol(x)
  beta = rep(0, k)
  
  while (diff > .000001) {
    mux = x %*% beta
    px = exp(mux) / (1 + exp(mux))
    w1 = as.numeric(px * (1 - px))
    W = diag(w1)
    Y = as.matrix((y - px))
    betachange = solve(t(x) %*% W %*% x) %*% (t(x) %*% Y)
    newbeta = beta + betachange
    beta = newbeta
    diff = sum(betachange ^ 2)
  }
  return(beta)
}

#parameter estimates
logit_NR(y=y, x=x1)


# Poisson Regression
rm(list = ls())

library(broom)
library(faraway)
data(gala)
y <- gala$Species
x1 <- gala[, -c(1,2,3)]


ll_pois <- function(y, x, par) {
  y = as.matrix(y)
  x = as.matrix(cbind(1, x))
  k = ncol(x)
  beta = par[1:k]
  mux = exp(x %*% beta)
  
  l = dpois(x = y, lambda = mux, log = T)
  
  return(-sum(l))
}

res <- optim(par = c(0,0,0,0,0), y = y, x = x1, fn = ll_pois, hessian = T)
tidy(res) # std err of estimates

# GLM Model fit
colnames(gala)
model <- glm(formula = Species~Elevation+Nearest+Scruz+Adjacent, family = "poisson", data = gala)

tidy(model) # for model summary
glance(model) # for std error and goodness of fit
augment(model) # for fitted values and residuals

tidy(res) ; tidy(model)



# Logistic Regression(WBCA data) 
rm(list = ls())
library(faraway)
library(broom)
library(tidyverse)
data(wbca)

#A
modl_a <- glm(formula = Class~., family = "binomial", data = wbca)
tidy(modl_a)
glance(modl_a)
summary(modl_a)

#B Best Model Selection
step(modl_a) # choose model with lowest AIC
ml_c <- glm(formula = Class ~ Adhes + BNucl + Chrom + Mitos + NNucl + Thick + UShap, 
            family = "binomial", data = wbca)
summary(ml_c)

#C Predict outcome & CI
newrow <- wbca[1,]
newrow[1,-1] <-  c(1, 1, 3, 2, 1, 1, 4, 1, 1)
newpred <- predict(ml_c,newdata=newrow,type="l",se.fit=TRUE)    # Predicted Y
lgtVals <- newpred$fit + 1.96*c(0,-1,1)*newpred$se.fit          # CI of Predicted Y
probVals <- round(1/(1+exp(-lgtVals)),4)                        # p(x)


#D bengin if p>0.5 and malignant if p<0.5 Compute no of errors
# 0 if malignant, 1 if benign in the data
predBenign <- if_else(predict(ml_c,type="response") > .5, 1, 0) 
table(predBenign,wbca$Class)

#E change the cutoff to 0.9 so that p<0.9 is classified as malignant and p>0.9 as benign
# 0 if malignant, 1 if benign in the data
predBenign <- if_else(predict(ml_c,type="response") > .9, 1, 0) 
table(predBenign,wbca$Class)


#F Split Data into Test & training set
testid <- seq(from = 3, to = 681, by=3)
train <- wbca[-testid,]
test <- wbca[testid,]

ml_f1 <- glm(formula = Class~., family = "binomial", data = train)

step(ml_f1, direction = "both")

ml_f2 <- glm(formula = Class ~ Adhes + BNucl + Chrom + Mitos + NNucl + Thick + UShap, 
             family = "binomial", data = train)

pred <- predict(ml_f2, newdata = test, type = "r")

# 0 if malignant, 1 if benign in the data
predBenign <- if_else(pred > .5, 1, 0) 
table(predBenign,test$Class)

predBenign <- if_else(pred > .9, 1, 0) 
table(predBenign,test$Class)