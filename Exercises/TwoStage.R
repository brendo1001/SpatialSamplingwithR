load(file="Voorst.RData")

d <- 4 #number of psu selections
m <- 10 #fixed number of ssu selections per psu-draw

ids<-sample.int(nrow(grdVoorst),size=d,replace=TRUE)
mypsusample<-grdVoorst[ids,psu]
i<-1
ssuIds <- NULL
for (psuId in mypsusample) {
  ssuId <- sample(x = which(grdVoorst[,psu] == psuId), size = m)
  ssuIds <- c(ssuIds, ssuId)
  i<-i+1
}
mysample <- grdVoorst[ssuId,]
psumeans <- tapply(mysample$z,INDEX=mysample$psu,FUN=mean)

meanz <- mean(psumeans)
varmeanz <- var(psumeans) / d

#Sampling variance of estimated mean with SI of same size
varmeanzSI <- var(grdVoorst$z)/(d*m)