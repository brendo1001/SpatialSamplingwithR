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

d <- 4 #number of psu selections
m <- 10 #fixed number of ssu selections per psu-draw

set.seed(314)
mysample <- twostage(sframe=grdVoorst,psu="psu",d=d,m=m)

#Estimate population mean and its standard error
psumeans <- tapply(mysample$z,INDEX=mysample$psu,FUN=mean)
(populationmean <- mean(psumeans))
varmean <- var(psumeans) / d
(semean <- sqrt(varmean))

#compute smpling variance of estimated mean with SI of same size
varmeanzSI <- var(grdVoorst$z)/(d*m)

#compute relative variance
(relvar <- varmeanSI/varmean)
