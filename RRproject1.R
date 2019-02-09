## Risk och reserv Projekt 1

library(MASS)

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

arrivals.daily <- aggregate(list(Arrivals=claims$ClaimDay),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), length)
cost.daily <- aggregate(list(Cost=claims$ClaimCost),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)

claims.daily<-merge(arrivals.daily,cost.daily)

claims.365<-subset(claims, select=c("ClaimType","ClaimDay"))
# Label to days in 1 Y 1-365
claims.365$ClaimDay365 <- claims.365$ClaimDay %% 365
claims.365$ClaimDay365[claims.365$ClaimDay365==0] <- 365


## Kontroll
#sum(claims$ClaimCost)
#sum(claims.daily$Cost)
#sum(claims.daily$Arrivals)
#summary(claims)
#summary(claims.daily)
#head(claims.daily, n=30)

claims.daily$MeanCost<-claims.daily$Cost/claims.daily$Arrivals
head(claims.daily, n=20)




## ARRIVALS

# Histogram 1Y dividided by 12 months
par(mfrow=c(2,1))
h1 <- claims.365$ClaimDay365[claims.365$ClaimType==1]
b1 <- seq(min(h1), max(h1), length.out = 13)
hist(h1, breaks=b1)
h2 <- claims.365$ClaimDay365[claims.365$ClaimType==2]
b2 <- seq(min(h2), max(h2), length.out = 13)
hist(h2, breaks=b2)

# From hist, less arrivals June-August
# Label to days in 1 Y 1-365
claims.daily$ClaimDay365 <- claims.daily$ClaimDay %% 365
claims.daily$ClaimDay365[claims.daily$ClaimDay365==0] <- 365

claims.daily$Summer <- 0
claims.daily$Summer[b1[5]<claims.daily$ClaimDay365&claims.daily$ClaimDay365<=b1[9]] <- 1

# Branch 1
lambda1s<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==1&claims.daily$Summer==1],"Poisson")$estimate
lambda1w<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==1&claims.daily$Summer==0],"Poisson")$estimate
# Branch 2
lambda2s<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==2&claims.daily$Summer==1],"Poisson")$estimate
lambda2w<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==2&claims.daily$Summer==0],"Poisson")$estimate

# par(mfrow=c(2,2))
# plot(rpois(100,lambda1s))
# plot(rpois(100,lambda1w))
# plot(rpois(100,lambda2s))
# plot(rpois(100,lambda2w))









days<-365
runs<-100
res_mat<-matrix(0,days,runs)
for(d in (1:days))
{
  z_vec<-rpois(runs,lambda1w)
  res_mat[d,]<-z_vec
}


### COST
head(claims.daily, n=20)

par(mfrow=c(2,1))
plot(claims.daily$ClaimDay[claims.daily$ClaimType==1],claims.daily$MeanCost[claims.daily$ClaimType==1])
plot(claims.daily$ClaimDay[claims.daily$ClaimType==2],claims.daily$MeanCost[claims.daily$ClaimType==2])

par(mfrow=c(2,1))
hist(claims.daily$MeanCost[claims.daily$ClaimType==1],100)
hist(claims.daily$MeanCost[claims.daily$ClaimType==2],100)

# Empirisk fördelningsfunktion
par(mfrow=c(2,1))
plot(ecdf(claims.daily$MeanCost[claims.daily$ClaimType==1]))
plot(ecdf(claims.daily$MeanCost[claims.daily$ClaimType==2]))

# Claims 1
summary(claims.daily$MeanCost[claims.daily$ClaimType==1])
x<-claims.daily$MeanCost[claims.daily$ClaimType==1]
hist(x,freq=F)
fit1<-fitdistr(x,"lognormal")$estimate
lines(dlnorm(0:max(x),fit1[1],fit1[2]),lwd=3)