load(file="Voorst.RData")

s1local <- grdVoorst$s1-min(grdVoorst$s1)
s2local <- grdVoorst$s2-min(grdVoorst$s2)
#set spacing of points within clusters
spacing <- 100 
s1f <- as.factor(s1local%%spacing)
s2f <- as.factor(s2local)
#construct clusters (E-W oriented transects within zones)
grdVoorst$cluster <- as.character(interaction(s1f,s2f,as.factor(grdVoorst$zone)))

grdVoorst$id <- seq(1:nrow(grdVoorst))

#now define function for cluster random sampling
cl <- function(sframe,d) {
  units<-sample.int(nrow(sframe),size=d,replace=TRUE)
  ids.pnt<-sframe$id[units]
  ids.cl <- sframe$cluster[units]
  mysample <- sframe[sframe$cluster %in% ids.cl,]
  mysample$start <- 0
  mysample$start[mysample$id%in%ids.pnt] <- 1
  return(mysample)
}


#construct strata from zones
grdVoorst$zonef <- as.factor(grdVoorst$zone)
levels(grdVoorst$zonef) <- rep(c("A","B","C"),each=2)
#compute stratum sizes
Nh <- tapply(grdVoorst$z,INDEX=grdVoorst$zonef,FUN=length)
#set number of cluster draws per stratum
dh <- c(2,2,2)
set.seed(314)
stratumlabels <- unique(grdVoorst$zonef)
mysample<-NULL
for (i in 1:3) {
  grdh <- grdVoorst[grdVoorst$zonef==stratumlabels[i],]
  mysampleh <- cl(sframe=grdh,d=dh[i])
  mysample <- rbind(mysample,mysampleh)
}

cellsize<-25
cls <- unique(mysample$cluster)
for (i in 1:length(cls)) {
  mysample$s1[mysample$cluster==cls[i]] <- mysample$s1[mysample$cluster==cls[i]]+runif(1,min=-cellsize/2,max=cellsize/2)
  mysample$s2[mysample$cluster==cls[i]] <- mysample$s2[mysample$cluster==cls[i]]+runif(1,min=-cellsize/2,max=cellsize/2)
}


clusterMeans<-tapply(mysample$z,INDEX=mysample$cluster,FUN=mean)

stratumofcluster <- tapply(mysample$zonef,INDEX=mysample$cluster,FUN=unique)
stratumMeans <- tapply(clusterMeans,INDEX=stratumofcluster,FUN=mean)
stratumVars <- tapply(clusterMeans,INDEX=stratumofcluster,FUN=var)

wh <- Nh/sum(Nh)
(estimatedMean <- sum(wh*stratumMeans))
estimatedVarMean <- sum(wh^2*stratumVars/dh)
(semean <- sqrt(estimatedVarMean))
