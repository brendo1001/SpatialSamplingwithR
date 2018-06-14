#load  data
load(file="CottonFarmUzbekistan.RData")
grdUzbekistan$EM <- exp(grdUzbekistan$lnEM)

#check whether size variable EM is strictly positive (> 0)
summary(grdUzbekistan$EM)

grdUzbekistan$prob<-grdUzbekistan$EM/sum(grdUzbekistan$EM)
summary(grdUzbekistan$prob)
#set number of draws
d<-40

#set seed so that results can be reproduced
set.seed(314)

N <- nrow(grdUzbekistan)

mysample<-sample.int(N,size=d,replace=TRUE,prob=grdUzbekistan$prob)
zexpand<-grdUzbekistan$ECe[mysample]/grdUzbekistan$prob[mysample]
totalz<-mean(zexpand)
(meanz<- totalz/N)
vartotalz<-var(zexpand)/d
(varmeanz<-vartotalz/N^2)


#Compute true sampling variance of estimated mean with SIR
(TruevarmeanzSI<-var(grdUzbekistan$ECe)/d)

#compute true sampling variance of estimated mean with ppswr
zexpand<-grdUzbekistan$ECe/grdUzbekistan$prob
(Truevarmeanzpps <- var(zexpand)/(d*N^2))

(gain <- TruevarmeanzSI/Truevarmeanzpps)