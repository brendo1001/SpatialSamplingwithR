library(sampling)

#load data
load(file="CottonFarmUzbekistan.RData")
grdUzbekistan$EM <- exp(grdUzbekistan$lnEM)

#construct strata
grdUzbekistan$stratum <- ifelse(grdUzbekistan$EM<50,1,2)


Nh<- table(grdUzbekistan$stratum)
wh<-Nh/sum(Nh)

n<-40
nh<-round(n*wh) #Nh is in order stratum 1 to 3, so nh has ame order

#select stratified random sample using function strata of package sampling
#NB give stratum sample sizes in the order of stratlabels
units<-strata(grdUzbekistan,stratanames="stratum",size=nh[unique(grdUzbekistan$stratum)])
mysample<-getdata(grdUzbekistan,units)

#compute true stratum means of covariate
truemeanxh<-tapply(grdUzbekistan$EM,INDEX=grdUzbekistan$stratum,FUN=mean)

#compute sample means of study variable and covariate per stratum 
meanzh<-tapply(mysample$ECe,INDEX=mysample$stratum,FUN=mean)
meanxh<-tapply(mysample$EM,INDEX=mysample$stratum,FUN=mean)

#Separate regression estimator

L<-length(unique(grdUzbekistan$stratum))
bh<-regressionestimateh<-varregressionestimateh<-numeric(length=L)
for (i in 1:L) {
  subsam<-subset(mysample,mysample$stratum==i)
  lmsample<-lm(ECe~EM,data=subsam)
  bh[i]<-coef(lmsample)[2]
  regressionestimateh[i]<-meanzh[i]+bh[i]*(truemeanxh[i]-meanxh[i]) #regression estimator of mean for stratum i
  varregressionestimateh[i]<-(1-nh[i]/Nh[i])*(sum(lmsample$residuals^2)/(nh[i]-2))/nh[i]
}

#compute the regression estimator for the study area as a weighted average of the regression estimates per stratum
print(sepregressionestimate<-sum(wh*regressionestimateh))
varsepregressionestimate<-sum(wh^2*varregressionestimateh)
(sdsepregressionestimate<-sqrt(varsepregressionestimate))

#Combined regression estimator

#With small sample sizes per stratum, say nh <10,  I  recommend the combined regression estimator

#first estimate mean by estimator for stratified simple random sampling, i.e do not use covariate x
zhatpi<-sum(wh*meanzh)

#estimate mean of covariate x
xhatpi<-sum(wh*meanxh)

#estimate mean of regression coefficients, see Cochran, p. 202
#NB I did not account for fpc in computing the stratum variances and covariances
varxh<-tapply(mysample$EM,INDEX=mysample$stratum,FUN=var) #variance of covariate per stratum
covarh<-numeric(length=length(wh))
for (i in 1:length(wh)) {
  covarh[i]<-cov(mysample$EM[mysample$stratum==i],mysample$ECe[mysample$stratum==i])
}
#covarh <- by(mysample[, c("EM","ECe")], mysample$stratum, function(x){cov(x$EM, x$ECe)})

bcomb<-sum(wh^2*covarh/nh)/sum(wh^2*varxh/nh)

#compute the combined regression estimator
(combregressionestimate<-zhatpi+bcomb*(mean(grdUzbekistan$EM)-xhatpi))

#approximate the sampling variance of the combined regression estimator

#first estimate intercept
acomb<-zhatpi-bcomb*mean(grdUzbekistan$EM)
#now compute residuals
mysample$residuals<-mysample$ECe-(acomb+bcomb*mysample$EM)

#estimate the sampling variances of the estimated stratum means of the residuals
varmeanresih<-numeric(length=L)
for (i in 1:L) {
  subsam<-subset(mysample,mysample$stratum==i)
  varmeanresih[i]<-(1-nh[i]/Nh[i])*(sum(subsam$residuals^2)/(nh[i]-2))/nh[i] #estimated variance of the mean of the residuals
}

#now compute the variance of the mean of the residuals for the study area
varcombregressionestimate<-sum(wh^2*varmeanresih)
(sdcombregressionestimate<-sqrt(varcombregressionestimate))