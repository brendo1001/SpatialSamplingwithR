library(sampling)
library(stratification)
library(ggplot2)

#load data
load(file="CottonFarmUzbekistan.RData")
dat <- grdUzbekistan

dat$EM <- exp(dat$lnEM)

#set number of strata
H<-10

#compute optimal strata
nclass<- H*100 #number of classes used in computing the histogram

#data must be sorted in ascending order by the columns used for optimal stratification, see help of function strata.cumrootf
dat <- dat[order(dat$EM),]

#NB n does not influence the optimal stratification
n <- 40
optstrata<-strata.cumrootf(x=dat$EM,n=n,Ls=H,nclass=nclass)

optstrata$bh #stratum boundaries
optstrata$Nh #stratum sizes

#add optimal strata to data.frame
dat$optstrata<-optstrata$stratumID

#plot strata
ggplot(data = dat) +
  geom_raster(mapping = aes(x = x1/1000, y = x2/1000, fill = factor(optstrata))) +
  scale_fill_discrete(name="Stratum") +
  scale_y_continuous(name = "Northing (km)") +
  scale_x_continuous(name = "Easting (km)") +
  coord_equal()


#select stratified simpe random sample

#stratum sample sizes in optstrata$nh. Default is Neyman allocation
(nh <- optstrata$nh)

units<-strata(dat,stratanames="optstrata",size=nh,method="srswor")
mysample<-getdata(dat,units)
meanzh<-tapply(mysample$ECe, INDEX=mysample$optstrata,FUN=mean)
wh <- optstrata$Nh/sum(optstrata$Nh)
(meanz<-sum(wh*meanzh))
S2h<-tapply(mysample$ECe, INDEX=mysample$optstrata,FUN=var)
(Vmeanz_STSI<-sum(wh^2*(1-nh/optstrata$Nh)*S2h/nh))

#The sampling variance can be computed without error from the simulated population
S2htrue<-tapply(dat$ECe, INDEX=dat$optstrata,FUN=var) 
(TrueVmeanz_STSI<-sum(wh^2*S2htrue/nh))

#compute gain in precision due to stratification (stratification effect)
TrueVmeanz_SI<-var(dat$ECe)/n
(gain<-TrueVmeanz_SI/TrueVmeanz_STSI)
