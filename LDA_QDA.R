wines <-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
             sep = ",")

names(wines) <-c("Class", "Alcohol", "Malic", "Ash", "Alcal", "Mg", "Phenol", 
                 "Flav", "Nonf", "Proan", "Color", "Hue", "Abs", "Proline")

colors <- c("green", "red" , "blue")[wines[, 1]]


## ----------------------------------------------------------------------------------------------
cov(wines[wines$Class == 1, -1])
cov(wines[wines$Class == 2, -1])
cov(wines[wines$Class == 3, -1])

pairs(
  wines[, -1],
  pch = 16,
  cex = .3,
  gap = 0,
  col = colors,
  xaxt = "n",
  yaxt = "n"
)


## ----------------------------------------------------------------------------------------------
library(MASS)
ld <- lda(Class ~ ., prior = c(.2, .4, .4), data = wines)
score <- as.matrix(wines[, 2:14]) %*% ld$scaling
plot(score, col = wines$Class)

pred <- predict(ld, data = wines)
table(predicted_class = pred$class,
      observed_class = wines$Class)


## ----------------------------------------------------------------------------------------------
qd <- qda(Class ~ ., data = wines)
scoreqd <- as.matrix(wines[, 2:14]) %*% qd$scaling[, , 1]
plot(scoreqd, col =wines[, 1])

predqd <- predict(qd, data = wines)
table(predicted_class = predqd$class,
      observed_class = wines$Class)


## ----------------------------------------------------------------------------------------------
library(readr)
dat <- read_csv("T11-4.csv")
ldbank <- lda(class ~ ., data = dat)
scorebank <- as.matrix(dat[,-5]) %*% ldbank$scaling

pred <- predict(ldbank, data = dat)
table(predicted_class = pred$class,
      observed_class = dat$class)

#p1=.05, p2=.95

ldbank <- lda(class ~ ., prior = c(.05, .95), data = dat)
scorebank <- as.matrix(dat[,-5]) %*% ldbank$scaling


pred <- predict(ldbank, data = dat)
table(predicted_class = pred$class,
      observed_class = dat$class)


## ----------------------------------------------------------------------------------------------
# c(1|2)=3,c(2|1)=7, p1=.05, p2=.95
#costpriorratio=8.142857, so, p2:p1=8.142857:1 or p1*8.142857 = p2
# p1+p2=1, p1(1+8.142857)=1, p1=1/9.142857,
# p1=0.109375, p2= 0.890625

ldbank <- lda(class ~ ., prior = c(0.10, 0.90), data = dat)
scorebank <- as.matrix(dat[,-5]) %*% ldbank$scaling


pred <- predict(ldbank, data = dat)
table(predicted_class = pred$class,
      observed_class = dat$class)


#  Box's M-test for Homogeneity of Covariance Matrices
library(biotools)
dat$class <- as.factor(dat$class)
boxM(dat[, -5], dat[, 5]) 
# covariance are unequal

