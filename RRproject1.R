## Risk och reserv Projekt 1

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)

claims$ClaimDay365 <- claims$ClaimDay %% 365
claims$ClaimDay365[claims$ClaimDay365==0] <- 365

# Histogram 1Y
par(mfrow=c(2,1))
h1 <- claims$ClaimDay365[claims$ClaimType==1]
b1 <- seq(min(h1), max(h1), length.out = 13)
hist(h1, breaks=b1)
h2 <- claims$ClaimDay365[claims$ClaimType==2]
b2 <- seq(min(h2), max(h2), length.out = 13)
hist(h2, breaks=b2)

claims$Summer <- 0
claims$Summer[b1[5]<claims$ClaimDay365&claims$ClaimDay365<b1[9]] <- 1

obs1 <- length(claims$ClaimCost[claims$ClaimType==1])
obs2 <- length(claims$ClaimCost[claims$ClaimType==2])
#Winter
obs1w <- length(claims$ClaimCost[claims$ClaimType==1&&claims$Summer==0])
obs2w <- length(claims$ClaimCost[claims$ClaimType==2&&claims$Summer==0])
#Summer
obs1s <- length(claims$ClaimCost[claims$ClaimType==1&&claims$Summer==1])
obs2s <- length(claims$ClaimCost[claims$ClaimType==2&&claims$Summer==1])


lambda1 <- obs1 / max(claims$ClaimDay[claims$ClaimType==1])
lambda2 <- obs2 / max(claims$ClaimDay[claims$ClaimType==1])

lambda1 <- obs1 / max(claims$ClaimDay[claims$ClaimType==1])
lambda2 <- obs2 / max(claims$ClaimDay[claims$ClaimType==1])


## ARRIVALS
claims1 <- subset(claims, ClaimType == 1, select=c(ClaimDay, ClaimDay365, ClaimCost))
claims2 <- subset(claims, ClaimType == 2, select=c(ClaimDay, ClaimDay365, ClaimCost))


C1 <- data.matrix(claims1)
C2 <- data.matrix(claims2)

# Histogram 1Y
par(mfrow=c(2,1))
hist(C1[,2],breaks = seq(min(C1[,2]), max(C1[,2]), length.out = 13))
hist(C2[,2],breaks = seq(min(C2[,2]), max(C2[,2]), length.out = 13))

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
