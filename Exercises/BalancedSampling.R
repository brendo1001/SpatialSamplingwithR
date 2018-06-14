#load  data
load(file="UzbekistanSimulations.RData")
grdUzbekistan$EM <- exp(grdUzbekistan$lnEM)

#set number of draws
n<-40

#set seed so that results can be reproduced
set.seed(31415)

N <- nrow(grdUzbekistan)

#define matrix with covariate for balancing; first column of matrix must be filled with ones
X<-cbind(rep(1,times=N),grdUzbekistan$EM)

#compute inclusion probabilities; use equal probabilities
pik<-rep(n/N,times=N)

#set number of samples
nsamples<-10

#start for loop
meanz <- Vmeanz <- numeric(length=nsamples)
for (i in 1:nsamples) {
  sampleind=samplecube(X=X,pik=pik,comment=FALSE,method=1)
  sam<-grdUzbekistan[sampleind==1,]
  meanz[i]<-sum(grdUzbekistan$z[sampleind==1]/pik[sampleind==1])/N
  z.lm<-lm(ECe~EM,data=sam)
  e<-residuals(z.lm)
  Vmeanz[i]<-(1-n/N)*var(e)*(n-1)/((n-2)*n)
}

#compute summary statistics
(meanmeanz<-mean(meanz))
(mean(grdUzbekistan$ECe))

(varmeanz<-var(meanz))
(meanVmeanz<-mean(Vmeanz))

#Compute sampling variance of estimated mean with SI
(TrueVmeanzSI<-var(grdUzbekistan$ECe)/n)

(gain<-TrueVmeanzSI/varmeanz)

zexpand<-grdUzbekistan$ECe/grdUzbekistan$prob

#compute true mean of ppswr
(TrueVmeanzpps <- var(zexpand)/(d*nrow(grdUzbekistan)^2))
