# IPL
https://github.com/sona-1999/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv
IPL Ball-by-Ball 2008-2020.csv

df.score = read.csv('IPL Ball-by-Ball 2008-2020.csv', header=TRUE)
names(df.score)
View(df.score)
dim(df.score)


library(dplyr)
runs  = df.score %>%
  group_by(batsman,id)%>%
  summarize(score = sum (batsman_runs))
View(runs)
unique(runs$batsman)
dim(runs)
names(df.score)


##BATSMAN

stokes =  runs[runs$batsman == 'BA Stokes',]
View(stokes)
hist(stokes$score, 6)

library(MASS)
library(fitdistrplus)
descdist(stokes$score)

###Weibull Distribution

##H0 = The Distribution follows Weibull
##H1 = The Distribution does not follow Weibull

res<-fitdistr(stokes$score+0.00001,"weibull",lower=0.001)
res$estimate
stokeswei = rweibull(1000, shape=0.7209374, scale = 20.1115283 )
plot(density(stokeswei))
ks.test(stokes$score,"pweibull", scale=20.1115283, shape=0.7209374 ) 

##since the p value is greater than 0.5 we accept the null hypothesis. That is, the distribution follows Weibull distribution

###Gamma Distribution

##H0 = The Distribution follows Gamma 
##H1 = The Distribution does not follow Gamma 


res1<-fitdistr(stokes$score+.001,"gamma",lower=0.001)
res1$estimate
stokesgam = rgamma(1000, shape= 0.66893219, rate =0.02908096)
plot(density(stokesgam))
ks.test(stokes$score+0.001,"gamma")

#since the p value is less than 0.5 we reject the null hypothesis. That is, the distribution does not follow gamma distribution


###Lognormal

##H0 = The Distribution follows Lognormal 
##H1 = The Distribution does not follow Lognormal

res<-fitdistr(stokes$score+0.0001,"Lognormal",lower=0.001)
res$estimate
ks.test(stokes$score,"plnorm",meanlog=2.111095, sdlog=2.808551)

#since the p value is less than 0.5 we reject the null hypothesis. That is, the distribution does not follow lognormal distribution



##Normal

##H0 = The Distribution follows Normal 
##H1 = The Distribution does not follow  Normal


res1<-fitdistr(stokes$score,"normal",lower=0.001)
res1$estimate
ks.test(stokes$score,"pnorm",mean=23.00000, sd =23.72973 )

#since the p value is greater than 0.5 we accept the null hypothesis. That is, the distribution  follows Normaldistribution


##Exponential

##H0 = The Distribution follows Exponential 
##H1 = The Distribution does not follow Exponential


res1<-fitdistr(stokes$score,"Exponential",lower=0.001)
res1$estimate
ks.test(stokes$score+0.001,"exp")

#since the p value is less than 0.5 we reject the null hypothesis. That is, the distribution does not follow Exponential distribution

##For weibull Distribution the p value is 0.1561. For normal distribution the p value is 0.105. since 0.1561 > 0.105 , the distribution follows weibull distribution



### BOWLER


wickets  = df.score %>%
  group_by(bowler,id)%>%
  summarize(wicket = sum(is_wicket))
View(wickets)
unique(wickets$bowler)
dim(wickets)

crd = wickets[wickets$bowler == 'CRD Fernando',]
hist(crd$wicket,freq=F,breaks=c(0,1,2,3,4,5),main="histogram of fernado")
m=mean(crd$wicket);std=sd(crd$wicket);m;std
lines(density(crd$wicket),col="blue")
library(vcd) 

## H0 = The distribution follows Poisson 
## H1 = The distribution does not  follows Poisson
gf.crd <- goodfit(crd$wicket, type = "poisson", par = NULL)
summary(gf.crd)
plot(gf.crd, main="fernado wicket distribution")

##Since the p value is greater than 0.05 we accept the null hypothesis. That is , the distribution follows Poisson distribution. 


