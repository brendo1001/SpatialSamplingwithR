load(file="Voorst.RData")

grdVoorst$s1 <- grdVoorst$s1-min(grdVoorst$s1)
grdVoorst$s2 <- grdVoorst$s2-min(grdVoorst$s2)

#define clusters: E-W oriented transects
clusterspacing <- 100 #spacing of points within clusters
s1f <- as.factor(grdVoorst$s1%%clusterspacing)
s2f <- as.factor(grdVoorst$s2)
grdVoorst$cluster <- as.character(interaction(s1f,s2f,as.factor(grdVoorst$zone)))

#compute size of clusters
clustersize <- tapply(grdVoorst$z,INDEX=grdVoorst$cluster,FUN=length)

N <- sum(clustersize)

set.seed(314)
d <- 4 #number of cluster draws
(ids.pnts<-sample.int(N,size=d,replace=TRUE))
(ids.clusters <- grdVoorst$cluster[ids.pnts])
(mysample <- grdVoorst[grdVoorst$cluster %in% ids.clusters,])

clmeanz<-tapply(mysample$z,INDEX=mysample$cluster,FUN=mean)

(meanz<-mean(clmeanz))

#compute the variance between the estimated cluster means
S2clmeanz<-var(clmeanz)

(semean<-sqrt(S2clmeanz/d))

#compute expected sample size
p <- clustersize/N

(mean.n <- d*sum(p*clustersize))
