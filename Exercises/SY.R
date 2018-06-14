library(sp)
library(spcosa)

load(file="Voorst.RData")

#change class to SpatialPixelsDataFrame
gridded(grdVoorst) <- ~s1+s2

#specify expected sample size
mean.n <- 40
set.seed(143)
mysamplecrds<-spsample(x=grdVoorst,n=mean.n,type="regular")

#overlay selected points with grdVoorst to extract values of z
mysample <- mysamplecrds%over%grdVoorst
mysample<-as(mysample,"data.frame")
mysample <- data.frame(coordinates(mysamplecrds),mysample)

(meanz<-mean(mysample$z))

#estimate sampling variance (standard error) as if sample were a simple random sample
#estimate the population variance
S2 <- var(mysample$z)

#compute sample size
n <- nrow(mysample)

(semeanSI<-sqrt(S2/n))

#estimate sampling variance as if sample were a stratified simple random sample from geostrata

#cluster the sampling units

#change class of object with sample data to SpatialPixelsDataFrame
gridded(mysample)<-~x1+x2

#compute number of strata from length of data file NB number of sampling units can be odd
H<-floor(n/2)

#cluster the sampling units
myStrata<-stratify(mysample,nStrata=H,equalArea=TRUE,nTry=100)
myStrata.df<-as(myStrata,"data.frame")

mysample<-cbind(data.frame(mysample),myStrata.df$stratumId)
names(mysample)[8]<-"stratumId"
S2h<-tapply(mysample$z,INDEX=mysample$stratumId,FUN=var)
nh<-tapply(mysample$z,INDEX=mysample$stratumId,FUN=length)
varmeanh<-S2h/nh
wh<-nh/sum(nh)
(semeanSTSI<-sqrt(sum(wh^2*varmeanh)))
