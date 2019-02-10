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

#head(cost.year,n=20)
rho.intra<-cor(cost.year$Cost[cost.year$ClaimType==1],
    cost.year$Cost[cost.year$ClaimType==2])
plot(cost.year$Cost[cost.year$ClaimType==1],
     cost.year$Cost[cost.year$ClaimType==2],
     main=paste("rho=",rho.intra))


# # Kontroll
# sum(cost.year$Cost[cost.year$ClaimType==1])
# sum(cost.year$Cost[cost.year$ClaimType==2])
# sum(claims.daily$Cost[claims.daily$ClaimType==1])
# sum(claims.daily$Cost[claims.daily$ClaimType==2])

# rho.common<-cor(cost.common$Cost[cost.common$ClaimType==1],
#     cost.common$Cost[cost.common$ClaimType==2])
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


### Copulas

# Simulations of cost after 1 Y
# S1 Poisson/lognormal
# S2 Poisson/Weibull
load("S.RData")
#head(S,n=20)


n <- 2000
rho<-0.5
# Create bivariate N[0,1]xN[0,1] w correlation rho
sigma <- matrix(c(1.0,  rho,
                  rho,  1.0), nrow=2)
x <- mvrnorm(n, mu=rep(0, 2), Sigma=sigma, empirical=TRUE)

# Transform to U[0,1] u=F(x)
u <- pnorm(x)

# Inverse of empirical cdf from sampled marginaldistribution 
C1<-quantile(S[,1],u[,1],type=4)
C2<-quantile(S[,2],u[,2],type=4)

z<-cbind(C1,C2)
pairs.panels(z)

### BOOTSTRAP

head(claims.daily,n=40)
claims.daily$Month<-claims.daily$ClaimDay365 %/% 31

claims.monthly<-aggregate(list(Cost=claims.daily$Cost),list(Month=claims.daily$Month, ClaimType=claims.daily$ClaimType), sum)

head(claims.monthly,n=40)

