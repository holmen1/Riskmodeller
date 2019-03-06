## Risk och reserv Projekt 1

library(MASS)

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

arrivals.daily <- aggregate(list(Arrivals=claims$ClaimDay),
                            list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType),
                            length)
cost.daily <- aggregate(list(Cost=claims$ClaimCost),
                        list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)

claims.daily<-merge(arrivals.daily,cost.daily)

claims.365<-subset(claims, select=c("ClaimType","ClaimDay"))
# Label to days in 1 Y 1-365
claims.365$ClaimDay365 <- claims.365$ClaimDay %% 365
claims.365$ClaimDay365[claims.365$ClaimDay365==0] <- 365

claims.daily$MeanCost<-claims.daily$Cost/claims.daily$Arrivals

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
claims.daily$Summer[b1[5]<claims.daily$ClaimDay365&claims.daily$ClaimDay365<=b1[9]] <-1

# Branch 1
lambda1s<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==1 &
                   claims.daily$Summer==1], "Poisson")$estimate
lambda1w<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==1 &
                   claims.daily$Summer==0], "Poisson")$estimate
# Branch 2
lambda2s<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==2 &
                   claims.daily$Summer==1], "Poisson")$estimate
lambda2w<-fitdistr(claims.daily$Arrivals[claims.daily$ClaimType==2 &
                   claims.daily$Summer==0],"Poisson")$estimate

### COST
par(mfrow=c(2,1))
plot(claims.daily$ClaimDay[claims.daily$ClaimType==1],
     claims.daily$MeanCost[claims.daily$ClaimType==1])
plot(claims.daily$ClaimDay[claims.daily$ClaimType==2],
     claims.daily$MeanCost[claims.daily$ClaimType==2])

par(mfrow=c(2,1))
hist(claims.daily$MeanCost[claims.daily$ClaimType==1],100)
hist(claims.daily$MeanCost[claims.daily$ClaimType==2],100)

# Empirisk fördelningsfunktion
par(mfrow=c(2,1))
plot(ecdf(claims.daily$MeanCost[claims.daily$ClaimType==1]))
plot(ecdf(claims.daily$MeanCost[claims.daily$ClaimType==2]))

# Claims 1
summary(claims.daily$MeanCost[claims.daily$ClaimType==1])
x1<-claims.daily$MeanCost[claims.daily$ClaimType==1]
hist(x1,freq=F)
fit1<-fitdistr(x1,"lognormal")$estimate
lines(dlnorm(0:max(x1),fit1[1],fit1[2]),lwd=3)

# Claims 2
summary(claims.daily$MeanCost[claims.daily$ClaimType==2])
x2<-claims.daily$MeanCost[claims.daily$ClaimType==2]
hist(x2,freq=F)
fit2<-fitdistr(x2,"weibull")$estimate
#fit2<-fitdistr(x,"weibull",list(shape = 10000, scale = 10), lower = 50)$estimate
lines(dweibull(min(x2):max(x2),fit2[1],fit2[2]),lwd=3)


## Q-Q check
n1 <- length(x1)
x1.sorted <- sort(x1,dec=FALSE)
plot(x1.sorted,qlnorm((1:n1)/(n1+1),fit1[1],fit1[2]),
     xlim=c(min(x1),max(x1)*0.55), ylim=c(min(x1),max(x1)*0.3),
     xlab="data",ylab="model",main="QQ lognormal")
lines(x1.sorted,x1.sorted)

n2 <- length(x2)
x2.sorted <- sort(x2,dec=FALSE)#asp=1
plot(x2.sorted,qweibull((1:n2)/(n2+1),fit2[1],fit2[2]),
     xlim=c(0,max(x2)*0.55), ylim=c(0,max(x2)*0.3),
     xlab="data",ylab="model",main="QQ weibull")
lines(x2.sorted,x2.sorted)

par(mfrow=c(1,2))
plot(x1.sorted,qlnorm((1:n1)/(n1+1),fit1[1],fit1[2]),
     xlim=c(min(x1),max(x1)*0.5), ylim=c(min(x1),max(x1)*0.5),
     xlab="data",ylab="model",main="lognormal")
lines(x1.sorted,x1.sorted)
plot(x2.sorted,qweibull((1:n2)/(n2+1),fit2[1],fit2[2]),
     xlim=c(0,max(x2)*0.5), ylim=c(0,max(x2)*0.5),
     xlab="data",main="weibull")
lines(x2.sorted,x2.sorted)

### Compound
months<-c(31,28,31,30,31,30,31,31,30,31,30,31)
summer<-sum(months[5:8])
winter<-sum(months[1:4])+sum(months[9:12])
runs<-10000
S<-matrix(0,runs,2)
for (k in 1:runs)
{
  # Branch 1 lognormal claims
  N1w<-sum(rpois(lambda1w,winter))
  N1s<-sum(rpois(lambda1s,summer))
  N1<-N1w+N1s
  Y1<-rlnorm(N1,fit1[1],fit1[2])
  S[k,1]<-sum(Y1)
  
  # Branch 2 Weibull claims
  N2w<-sum(rpois(lambda2w,winter))
  N2s<-sum(rpois(lambda2s,summer))
  N2<-N2w+N2s
  Y2<-rweibull(N2,fit2[1],fit2[2])
  S[k,2]<-sum(Y2)
}

par(mfrow=c(2,1))
hist(S[,1],100)
hist(S[,2],100)

plot(S[,1],S[,2])
cor(S[,1],S[,2])

#save(claims.daily, fit1, fit2, lambda1s,lambda1w,lambda2s,lambda2w, file = "fitted.RData")
#save(S, file = "S.RData")






