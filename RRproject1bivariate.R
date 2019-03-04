## Risk och reserv Projekt 1
library(psych)

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")

# Olika värdedagar claim1 ? claim2 = inner join
CommonDays<-intersect(claims$ClaimDay[claims$ClaimType==1],
                      claims$ClaimDay[claims$ClaimType==2])

cost.daily <- aggregate(list(ClaimCost=claims$ClaimCost),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)

#Kontroll korrelation
D <- matrix(0,length(CommonDays),2)
for (k in 1:length(CommonDays))
{
  D[k,1]<-cost.daily$ClaimCost[cost.daily$ClaimDay==CommonDays[k] & cost.daily$ClaimType==1]
  D[k,2]<-cost.daily$ClaimCost[cost.daily$ClaimDay==CommonDays[k] & cost.daily$ClaimType==2]
}
rho.daily<-cor(D[,1],D[,2]) #= 0.2771018
plot(D[,1],D[,2],main=paste("rho.daily=",rho.daily))


# Yearly claim cost
claims.yearly <- claims
claims.yearly$ClaimYear <- (claims.yearly$ClaimDay %/% 365) + 1

cost.yearly <- aggregate(list(ClaimCost=claims.yearly$ClaimCost),list(ClaimYear=claims.yearly$ClaimYear, ClaimType=claims.yearly$ClaimType), sum)

Y <- matrix(0,10,2)
for (k in 1:10)
{
  Y[k,1] <- cost.yearly$ClaimCost[cost.yearly$ClaimYear==k & cost.yearly$ClaimType==1]
  Y[k,2] <- cost.yearly$ClaimCost[cost.yearly$ClaimYear==k & cost.yearly$ClaimType==2]
}
rho.yearly<-cor(Y[,1],Y[,2]) #= 0.497026
plot(Y[,1],Y[,2],main=paste("rho.yearly=",rho.yearly))


### Copulas

# Simulations of cost after 1 Y
# S1 Poisson/lognormal
# S2 Poisson/Weibull
load("S.RData")
#head(S,n=20)


n <- 100000
#rho<-0.5
# Create bivariate N[0,1]xN[0,1] w correlation rho
sigma <- matrix(c(1.0,  rho.yearly,
                  rho.yearly,  1.0), nrow=2)
x <- mvrnorm(n, mu=rep(0, 2), Sigma=sigma, empirical=TRUE)

# Transform to U[0,1]xU[0,1] u=F(x)
u <- pnorm(x)

# Inverse of empirical cdf from sampled marginaldistribution 
S1<-quantile(S[,1],u[,1],type=4)
S2<-quantile(S[,2],u[,2],type=4)


#save(S1,S2, file = "C.RData")

z<-cbind(S1,S2)
pairs.panels(z)




