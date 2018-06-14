library(ggplot2)

load(file="CottonFarmUzbekistan.RData")
grdUzbekistan$EM <- exp(grdUzbekistan$lnEM)

#set sample sizes
n1 <- 80
n2 <- 40
ids<-sample.int(nrow(grdUzbekistan),size=n1,replace=FALSE)
mysample <- grdUzbekistan[ids,]

#now subsample the selected sample and observe the target variable
ids2 <- sample.int(nrow(mysample), size = n2, replace = FALSE)
mysubsample<-mysample[ids2,]

lmsample<-lm(ECe~EM,data=mysubsample)
ab <- coef(lmsample)

#compute population mean of covariate and sample means of target variable and covariate
(samplemeanx<-mean(mysample$EM))
(subsamplemeanx <- mean(mysubsample$EM))
(subsamplemeanz <- mean(mysubsample$ECe))

#now compute regression estimator for two-phase sampling
(regressionestimate2phase<-subsamplemeanz+ab[2]*(samplemeanx-subsamplemeanx))

residualvariance<-(sum(lmsample$residuals^2)/(n2-2))

#approximate the sampling variance of the regression estimator
(varregressionestimate2phase<-var(subsample$z)/n1 + (1-n2/n1)* residualvariance/n2)


NUMBER_OF_SAMPLES <- 10000
RegressionEstimator2phase<-numeric(length=NUMBER_OF_SAMPLES)

set.seed(31415)

for (i in 1:NUMBER_OF_SAMPLES) {
  ids<-sample.int(nrow(grdUzbekistan),size=n1,replace=FALSE)
  mysample <- grdUzbekistan[ids,]
  ids2 <- sample.int(nrow(mysample), size = n2, replace = FALSE)
  mysubsample<-mysample[ids2,]
  
  lmsample<-lm(ECe~EM,data=mysubsample)
  ab <- coef(lmsample)
  samplemeanx<-mean(mysample$EM)
  subsamplemeanx <- mean(mysubsample$EM)
  subsamplemeanz <- mean(mysubsample$ECe)
  
  RegressionEstimator2phase[i]<-subsamplemeanz+ab[2]*(samplemeanx-subsamplemeanx)
}

(var(RegressionEstimator2phase))