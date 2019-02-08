## Risk och reserv Projekt 1

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)

## ARRIVALS

# Aggregate to days in 1 Y
claims$ClaimDay365 <- claims$ClaimDay %% 365
claims$ClaimDay365[claims$ClaimDay365==0] <- 365

# Histogram 1Y dividided by 12 months
par(mfrow=c(2,1))
h1 <- claims$ClaimDay365[claims$ClaimType==1]
b1 <- seq(min(h1), max(h1), length.out = 13)
hist(h1, breaks=b1)
h2 <- claims$ClaimDay365[claims$ClaimType==2]
b2 <- seq(min(h2), max(h2), length.out = 13)
hist(h2, breaks=b2)

# From hist, less arrivals June-August
claims$Summer <- 0
claims$Summer[b1[5]<claims$ClaimDay365&claims$ClaimDay365<=b1[9]] <- 1

#Winter
obs1w <- length(claims$ClaimCost[claims$ClaimType==1&claims$Summer==0])
obs2w <- length(claims$ClaimCost[claims$ClaimType==2&claims$Summer==0])
#Summer
obs1s <- length(claims$ClaimCost[claims$ClaimType==1&claims$Summer==1])
obs2s <- length(claims$ClaimCost[claims$ClaimType==2&claims$Summer==1])

# Branch 1
lambda1w <- obs1w / max(claims$ClaimDay[claims$ClaimType==1&claims$Summer==0])
lambda1s <- obs1s / max(claims$ClaimDay[claims$ClaimType==1&claims$Summer==1])
# Branch 2
lambda2w <- obs2w / max(claims$ClaimDay[claims$ClaimType==2&claims$Summer==0])
lambda2s <- obs2s / max(claims$ClaimDay[claims$ClaimType==2&claims$Summer==1])


days<-365
runs<-100
res_mat<-matrix(0,days,runs)
for(d in (1:days))
{
  z_vec<-rpois(runs,lambda1w)
  res_mat[d,]<-z_vec
}


plot(rpois(365,lambda1w),type="p")
plot(cumsum(rpois(365,lambda1w)))





# COST
claimsCount1Y <- aggregate(list(n=claims$ClaimDay365),list(ClaimType=claims$ClaimType,ClaimDay365=claims$ClaimDay365), length)
check1 <- sum(claimsCount1Y$n[claimsCount1Y$ClaimType==1])

claimsCountY1 <- subset(claimsCount1Y, ClaimType == 1, select=c(ClaimDay365, n))
claimsCountY2 <- subset(claimsCount1Y, ClaimType == 2, select=c(ClaimDay365, n))

par(mfrow=c(2,2))
plot(claimsCountY1)
plot(claimsCountY2)
plot(rpois(365,lambda1))
plot(rpois(365,lambda2))


## COST
claimsMeanCost10Y <- aggregate(list(MeanCost=claims$ClaimCost),list(ClaimType=claims$ClaimType,ClaimDay=claims$ClaimDay), mean)
claimsMeanCost1Y <- aggregate(list(MeanCost=claims$ClaimCost),list(ClaimType=claims$ClaimType,ClaimDay365=claims$ClaimDay365), mean)


# claimsCost1 <- subset(claimsMeanCost10Y, ClaimType == 1, select=c(ClaimDay, MeanCost))
# claimsCost2 <- subset(claimsMeanCost10Y, ClaimType == 2, select=c(ClaimDay, MeanCost))
# 
# par(mfrow=c(2,1))
# plot(claimsCost1)
# plot(claimsCost2)

claimsCostY1 <- subset(claimsMeanCost1Y, ClaimType == 1, select=c(ClaimDay365, MeanCost))
claimsCostY2 <- subset(claimsMeanCost1Y, ClaimType == 2, select=c(ClaimDay365, MeanCost))

par(mfrow=c(2,1))
plot(claimsCostY1)
plot(claimsCostY2)
