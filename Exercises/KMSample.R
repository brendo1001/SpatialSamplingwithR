library(fields)

# read data
load(file="HunterValley4Practicals.RData")

# K-means sampling

n<-20
set.seed(314)
myClusters <- kmeans(scale(grdHunterValley[,c(3,4,5,6,7)]), centers=n, iter.max=1000,nstart=10)
#Select locations closest to the centers of the clusters
rdist.out <- rdist(x1=myClusters$centers,x2=scale(grdHunterValley[,c(3,4,5,6,7)]))
units <- apply(rdist.out,MARGIN=1,which.min)
myKMSample <- grdHunterValley[units,c(3,4,5,6,7)]

#Compute MSSSD

#Compute population means and sds used above in scale, so that the covariate values in the sample are scaled with the same means and sds
populationmeans <- apply(grdHunterValley[,c(3,4,5,6,7)],MARGIN=2,FUN=mean)
populationsds <- apply(grdHunterValley[,c(3,4,5,6,7)],MARGIN=2,FUN=sd)
rdist.out <- rdist(x1=scale(grdHunterValley[,c(3,4,5,6,7)]),x2=scale(myKMSample,center=populationmeans,scale=populationsds))
dmin <- apply(rdist.out,MARGIN=1,min)
MSSSD_km <- mean(dmin^2)

#Fuzzy k-means sampling

#Scaled Euclidian distances, correlation of covariates not accounted for
#Read memberships computed with FuzMe
m_dia <- read.csv(file="20_class_Diag.txt",sep="")
m_dia <- m_dia[,-c(1,2,3)]

#Select locations with largest membership in cluster 1...k
units <- apply(m_dia,MARGIN=2,FUN=which.max)
myFKMSample_dia <- grdHunterValley[units,c(3,4,5,6,7)]

#Compute MSSSD
rdist.out <- rdist(x1=scale(grdHunterValley[,c(3,4,5,6,7)]),x2=scale(myFKMSample_dia,center=populationmeans,scale=populationsds))
dmin <- apply(rdist.out,MARGIN=1,min)
MSSSD_fkm_dia <- mean(dmin^2)

#Mahalanobis distance
m_mah <- read.csv(file="20_class_Maha.txt",sep="")
m_mah <- m_mah[,-c(1,2,3)]

units <- apply(m_mah,MARGIN=2,FUN=which.max)
myFKMSample_mah <- grdHunterValley[units,c(3,4,5,6,7)]

rdist.out <- rdist(x1=scale(grdHunterValley[,c(3,4,5,6,7)]),x2=scale(myFKMSample_mah,center=populationmeans,scale=populationsds))
dmin <- apply(rdist.out,MARGIN=1,min)
MSSSD_fkm_mah <- mean(dmin^2)