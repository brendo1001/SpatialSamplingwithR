#load data
load(file="CottonFarmUzbekistan.RData")
grdUzbekistan$EM <- exp(grdUzbekistan$lnEM)

#select simple random sample
n<-40
ids<-sample.int(nrow(grdUzbekistan),size=n,replace=FALSE)
mysample <- grdUzbekistan[ids,]

#fit simple linear model
lmsample<-lm(ECe~EM,data=mysample)
ab <- coef(lmsample)

#compute population mean of covariate and sample means of target variable and covariate
(populationmeanx<-mean(grdUzbekistan$EM))
(samplemeanx <- mean(mysample$EM))
(samplemeanz <- mean(mysample$ECe))

#now compute regression estimator
(regressionestimate<-samplemeanz+ab[2]*(populationmeanx-samplemeanx))

#approximate the sampling variance of the regression estimator
residualvariance<-(sum(lmsample$residuals^2)/(n-2))
(varregressionestimate<-residualvariance/n)

NUMBER_OF_SAMPLES <- 10000
RegressionEstimator<-numeric(length=NUMBER_OF_SAMPLES)

set.seed(314)
for (i in 1:NUMBER_OF_SAMPLES){
  ids<-sample.int(nrow(grdUzbekistan),size=n,replace=FALSE)
  mysample <- grdUzbekistan[ids,]

  #fit simple linear model
  lmsample<-lm(ECe~EM,data=mysample)
  ab <- coef(lmsample)
  samplemeanx <- mean(mysample$EM)
  samplemeanz <- mean(mysample$ECe)
  
  RegressionEstimator[i]<-samplemeanz+ab[2]*(populationmeanx-samplemeanx)
}

(varRegressionEstimator <- var(RegressionEstimator))

#compare with sampling variance of Horvitz-Thompson estimator
(varHTEstimator<-var(grdUzbekistan$ECe)/n)

(gain <- varHTEstimator/varRegressionEstimator)