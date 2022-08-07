##  Principal component

dat<-read.csv("fastfood behaviour.csv")

dat <- dat[, 1:30]
head(dat)
cor<-cor(dat)
eigen(cor)$values

pc.cr<-prcomp(dat, center = T, scale. = T) # cor =T
summary(pc.cr)
pc.cr$rotation

plot(pc.cr, type="l")
abline(h=1)


#factor analysis
library(psych)
pc <- principal(dat,nfactors = 6,covar=F)
fa.diagram(pc)

#scrorecom5<-0.6*dat$RQ.4.5+0.5*dat$RQ.6.5+.4*dat$RQ.2.5+0.4*dat$RQ.5.5

factoran <- fa(dat, nfactors = 6, fm="pa")
fa.diagram(factoran)
