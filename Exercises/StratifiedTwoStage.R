load(file="Voorst.RData")

twostage <- function(sframe,psu,d,m) {
  units<-sample.int(nrow(sframe),size=d,replace=TRUE)
  mypsusample<-sframe[units,psu]
  i<-1
  ssunits <- NULL
  for (psunit in mypsusample) {
    ssunit <- sample(x = which(sframe[,psu] == psunit), size = m, replace=TRUE)
    ssunits <- c(ssunits, ssunit)
    i<-i+1
  }
  mysample <- sframe[ssunits,]
  return(mysample)
}

#construct strata from zones
grdVoorst$zonef <- as.factor(grdVoorst$zone)
levels(grdVoorst$zonef) <- rep(c("A","B","C"),each=2)

#compute stratum sizes
Nh <- tapply(grdVoorst$z,INDEX=grdVoorst$zonef,FUN=length)

#set number of psu draws per stratum
dh <- c(2,2,2)

#set number of ssu selections per psu-draw
m <- 6 

set.seed(3141)
stratumlabels <- unique(grdVoorst$zonef)
mysample<-NULL
for (i in 1:3) {
  grdh <- grdVoorst[grdVoorst$zonef==stratumlabels[i],]
  mysampleh <- twostage(sframe=grdh,psu="psu",d=dh[i],m=m)
  mysample <- rbind(mysample,mysampleh)
}

#Estimate population mean and its standard error
psuMeans<-tapply(mysample$z,INDEX=mysample$psu,FUN=mean)

stratumofpsu <- tapply(mysample$zonef,INDEX=mysample$psu,FUN=unique)
stratumMeans <- tapply(psuMeans,INDEX=stratumofpsu,FUN=mean)
stratumVars <- tapply(psuMeans,INDEX=stratumofpsu,FUN=var)

wh <- Nh/sum(Nh)
(estimatedMean <- sum(wh*stratumMeans))
(estimatedVarMean <- sum(wh^2*stratumVars/dh))
(semean <- sqrt(estimatedVarMean))