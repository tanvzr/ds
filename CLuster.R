library(robustbase) # for data
data(coleman)
dat<-coleman

d<-dist(dat[,-6])

cluster_s<-hclust(d, method = "single")
plot(cluster_s)

cluster_c<-hclust(d,method = "complete")
plot(cluster_c)

cluster_a<-hclust(d,method = "average")
plot(cluster_a)

cluster_ward <- hclust(d, method="ward.D")
plot(cluster_ward)


### distancing

cluster_c <- hclust(d, method="complete")
plot(cluster_c)
rect.hclust(cluster_c, k=3, border="red")

groups <- cutree(cluster_c, k=3) 


### variable clustering

library(factoextra)
datscaled<-scale(dat[,-6])
dist_cor_scaled<- get_dist(datscaled, method = "pearson")
varc_scaled<-hclust(dist_cor_scaled, method ="complete")
plot(varc_scaled)

dist_cor<- get_dist(dat[,-6], method = "pearson")
varc<-hclust(dist_cor, method ="complete")
plot(varc)


## variable clustering using transpose
data_transpose<-t(dat[,-6])
dist_cor_var<- get_dist(data_transpose, method = "pearson")
varc_var<-hclust(dist_cor_var, method ="complete")
plot(varc_var)

## variable clustering using correlation as distance
cord<-as.matrix(cor(dat[,-6]))

cord<-1-cord
cord[upper.tri(cord)]<-NA
diag(cord)<-0

cord1<- as.dist(cord, diag = TRUE)

varc_use_correlation<-hclust(cord1, method ="complete")
plot(varc_use_correlation)








### k means

kmeancluster<-kmeans(dat[,-6],3)

library(cluster)
clusplot(x = dat[,-6], clus = kmeancluster$cluster, color=TRUE, shade=FALSE,
         labels=2, lines=0)


### within group sum of sq

wss <- (nrow(dat[,-6])-1)*sum(apply(dat[,-6],2,var))
for (i in 2:8){
  wss[i] <- sum(kmeans(dat[,-6],i)$withinss)
}   
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


### Gap Statistic
library(cluster)
gap <- clusGap(dat[,-6], kmeans, K.max=10, B=500)
k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")
fit <- kmeans(dat[,-6], k)

# Plot the results.
plot(gap, main="GAP Statistics")
abline(v=k)
