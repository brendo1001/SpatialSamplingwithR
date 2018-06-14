library(stratification)

load(file="Voorst.RData")

# compute total number of pixels per stratum and stratum weights (relative size)
Nh<-tapply(X=grdVoorst$stratum, INDEX = grdVoorst$stratum, FUN =length) 
wh<-Nh/sum(Nh)

#total sample size
n <- 40

# compute stratum sample sizes for proportional allocation 
nh <- round(n * wh)
#sum of stratum sample sizes is 41, we want 40, so we reduce largest stratum sample size by 1
nh[1] <- nh[1] - 1

#reorder the stratum sample sizes so that it can be used in function strata below
nh.order <- nh[unique(grdVoorst$stratum)]

set.seed(314)
units<-strata(grdVoorst,stratanames="stratum",size=nh.order,method="srswor")
mysample<-getdata(grdVoorst,units)

meanh<-tapply(mysample$z,INDEX=mysample$stratum,FUN=mean)
S2h<-tapply(mysample$z,INDEX=mysample$stratum,FUN=var)
varmeanh<-S2h/nh

(estimatedmean<-sum(wh*meanh))
varmean <- sum(wh^2*varmeanh)
(semean<-sqrt(varmean))

#compute lower and upper bound of 95% confidence interval using the t distribution
df <- n - length(Nh)
(lower<-estimatedmean-qt(0.975,df,lower.tail=TRUE)*semean)
(upper<-estimatedmean+qt(0.975,df,lower.tail=TRUE)*semean)

#check whether 95% confidence interval covers the true mean
(populationmean <- mean(grdVoorst$z))
(ind<-(populationmean >lower & populationmean < upper))
