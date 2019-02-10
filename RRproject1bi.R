## Risk och reserv Projekt 1

library(MASS)

load("fitted.RData")
head(claims.daily,n=20)

length(claims.daily$Cost[claims.daily$ClaimType==1])
length(claims.daily$Cost[claims.daily$ClaimType==2])

CommonDays<-intersect(claims.daily$ClaimDay[claims.daily$ClaimType==1],
                      claims.daily$ClaimDay[claims.daily$ClaimType==2])

cost.common <- subset(claims.daily, claims.daily$ClaimDay %in% CommonDays, 
                  select=c(ClaimDay,ClaimDay365, ClaimType, Cost))
cost.year<-aggregate(list(Cost=cost.common$Cost),list(ClaimDay365=cost.common$ClaimDay365, ClaimType=cost.common$ClaimType), sum)

head(cost.year,n=20)
plot(cost.year$Cost[cost.year$ClaimType==1],
     cost.year$Cost[cost.year$ClaimType==2])
cor(cost.year$Cost[cost.year$ClaimType==1],
     cost.year$Cost[cost.year$ClaimType==2])

# Kontroll
sum(cost.year$Cost[cost.year$ClaimType==1])
sum(cost.year$Cost[cost.year$ClaimType==2])
sum(claims.daily$Cost[claims.daily$ClaimType==1])
sum(claims.daily$Cost[claims.daily$ClaimType==2])

plot(cost.common$Cost[cost.common$ClaimType==1],
     cost.common$Cost[cost.common$ClaimType==2])

cor(cost.common$Cost[cost.common$ClaimType==1],
     cost.common$Cost[cost.common$ClaimType==2])

head(cost.common,n=20)
cost.common$Year<-cost.common$ClaimDay %/% 365
head(cost.common,n=2000)

cost.yearend<-aggregate(list(Cost=cost.common$Cost),list(ClaimYear=cost.common$Year, ClaimType=cost.common$ClaimType), sum)
plot(cost.yearend$Cost[cost.yearend$ClaimType==1&cost.yearend$ClaimYear<10],
     cost.yearend$Cost[cost.yearend$ClaimType==2&cost.yearend$ClaimYear<10])

cor(cost.yearend$Cost[cost.yearend$ClaimType==1&cost.yearend$ClaimYear<10],
     cost.yearend$Cost[cost.yearend$ClaimType==2&cost.yearend$ClaimYear<10])

### Copulas

# Simulations of cost after 1 Y
# S1 Poisson/lognormal
# S2 Poisson/Weibull
load("S.RData")
head(S,n=20)



m <- 2
n <- 200
mu    <- rep(0, m)
sigma <- matrix(c(1.0,  0.5,
                  0.5,  1.0), nrow=2)

x <- mvrnorm(n, mu=mu, Sigma=sigma, empirical=TRUE)

u <- pnorm(x)
#head(u,23)
F1inv<-quantile(S[,1],u[,1],type=4)
F2inv<-quantile(S[,2],u[,2],type=4)

plot(F1inv,F2inv)
cor(F1inv,F2inv)


