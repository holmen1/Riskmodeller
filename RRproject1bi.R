## Risk och reserv Projekt 1

library(MASS)
library(psych)

load("fitted.RData")
# head(claims.daily,n=20)
# length(claims.daily$Cost[claims.daily$ClaimType==1])
# length(claims.daily$Cost[claims.daily$ClaimType==2])

# Olika värdedagar claim1 ? claim2 = inner join
CommonDays<-intersect(claims.daily$ClaimDay[claims.daily$ClaimType==1],
                      claims.daily$ClaimDay[claims.daily$ClaimType==2])
cost.common <- subset(claims.daily, claims.daily$ClaimDay %in% CommonDays, 
                  select=c(ClaimDay,ClaimDay365, ClaimType, Cost))
cost.year<-aggregate(list(Cost=cost.common$Cost),list(ClaimDay365=cost.common$ClaimDay365, ClaimType=cost.common$ClaimType), sum)


#Omkontroll korrelation
s1<-subset(cost.common, cost.common$ClaimType==1,select=c(ClaimDay,Cost))
s2<-subset(cost.common, cost.common$ClaimType==2,select=c(ClaimDay,Cost))
Y<-matrix(0,nrow(cost.common),2)
i<-1
for (k in s1$ClaimDay)
{
  print(k)
  Y[i,1]<-s1$Cost[s1$ClaimDay==k]
  Y[i,2]<-s2$Cost[s2$ClaimDay==k]
  i=i+1
}
rho.total<-cor(Y[,1],Y[,2]) #= 0.5277435
plot(Y[,1],Y[,2],main=paste("rho=",rho.total))

# # Kontroll
# sum(cost.year$Cost[cost.year$ClaimType==1])
# sum(cost.year$Cost[cost.year$ClaimType==2])
# sum(claims.daily$Cost[claims.daily$ClaimType==1])
# sum(claims.daily$Cost[claims.daily$ClaimType==2])


# plot(cost.common$Cost[cost.common$ClaimType==1],
#      cost.common$Cost[cost.common$ClaimType==2],
#      main=paste("rho=",rho.common))

# Yearly claim cost
cost.common$Year<-cost.common$ClaimDay %/% 365
#head(cost.common,n=2000)
cost.yearend<-aggregate(list(Cost=cost.common$Cost),list(ClaimYear=cost.common$Year, ClaimType=cost.common$ClaimType), sum)

rho.yearend<-cor(cost.yearend$Cost[cost.yearend$ClaimType==1&cost.yearend$ClaimYear<10],
    cost.yearend$Cost[cost.yearend$ClaimType==2&cost.yearend$ClaimYear<10])
plot(cost.yearend$Cost[cost.yearend$ClaimType==1&cost.yearend$ClaimYear<10],
     cost.yearend$Cost[cost.yearend$ClaimType==2&cost.yearend$ClaimYear<10],
     main=paste("rho=",rho.yearend))

#Omkontroll korrelation
s1<-subset(cost.yearend, cost.yearend$ClaimType==1,select=c(ClaimYear,Cost))
s2<-subset(cost.yearend, cost.yearend$ClaimType==2,select=c(ClaimYear,Cost))
Y2<-matrix(0,9,2)
for (k in 1:9)
{
  Y2[k,1]<-s1$Cost[s1$ClaimYear==k]
  Y2[k,2]<-s2$Cost[s2$ClaimYear==k]
}
#plot(Y[,1],Y[,2])
rho.yearend.ny<-cor(Y2[,1],Y2[,2])
plot(Y2[,1],Y2[,2])

#Why?
par(mfrow=c(1,2))
plot(cost.yearend$Cost[cost.yearend$ClaimType==1&cost.yearend$ClaimYear<10],
     cost.yearend$Cost[cost.yearend$ClaimType==2&cost.yearend$ClaimYear<10],
     main=paste("rho=",rho.yearend))
plot(Y2[,1],Y2[,2],main=paste("rho=",rho.yearend.ny))


### Copulas

# Simulations of cost after 1 Y
# S1 Poisson/lognormal
# S2 Poisson/Weibull
load("S.RData")
#head(S,n=20)


n <- 1000
#rho<-0.5
# Create bivariate N[0,1]xN[0,1] w correlation rho
sigma <- matrix(c(1.0,  rho.total,
                  rho.total,  1.0), nrow=2)
x <- mvrnorm(n, mu=rep(0, 2), Sigma=sigma, empirical=TRUE)

# Transform to U[0,1]xU[0,1] u=F(x)
u <- pnorm(x)

# Inverse of empirical cdf from sampled marginaldistribution 
S1<-quantile(S[,1],u[,1],type=4)
S2<-quantile(S[,2],u[,2],type=4)


#save(S1,S2, file = "C.RData")

z<-cbind(S1,S2)
pairs.panels(z)




