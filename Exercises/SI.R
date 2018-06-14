load(file="Voorst.RData")

n <- 40
N<-nrow(grdVoorst)
set.seed(314)
ids <- sample.int(N, size = n, replace = FALSE)
mysample <- grdVoorst[ids,]
(estimatedmean <- mean(mysample$z))

varmean <- var(mysample$z)/n
(semean<-sqrt(varmean))

#compute lower and upper bound of 95% confidence interval using the t distribution
(lower<-estimatedmean-qt(0.975,n-1,lower.tail=TRUE)*semean)
(upper<-estimatedmean+qt(0.975,n-1,lower.tail=TRUE)*semean)

#check whether 95% confidence interval covers the true mean
(populationmean <- mean(grdVoorst$z))
(ind<-(populationmean >lower & populationmean < upper))