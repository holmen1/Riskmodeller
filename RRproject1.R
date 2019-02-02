## Risk och reserv Projekt 1

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")
claims$ClaimDay365 <- claims$ClaimDay %% 366

## ARRIVALS
claims1 <- subset(claims, ClaimType == 1, select=c(ClaimDay, ClaimDay365, ClaimCost))
claims2 <- subset(claims, ClaimType == 2, select=c(ClaimDay, ClaimDay365, ClaimCost))


C1 <- data.matrix(claims1)
C2 <- data.matrix(claims2)

# Histogram 10Y
par(mfrow=c(2,1))
hist(C1[,1])
hist(C2[,1])

# Histogram 1Y
par(mfrow=c(2,1))
hist(C1[,2])
hist(C2[,2])

## COST
claimsMeanCost10Y <- aggregate(list(MeanCost=claims$ClaimCost),list(ClaimType=claims$ClaimType,ClaimDay=claims$ClaimDay), mean)
claimsMeanCost1Y <- aggregate(list(MeanCost=claims$ClaimCost),list(ClaimType=claims$ClaimType,ClaimDay365=claims$ClaimDay365), mean)

claimsCost1 <- subset(claimsMeanCost10Y, ClaimType == 1, select=c(ClaimDay, MeanCost))
claimsCost2 <- subset(claimsMeanCost10Y, ClaimType == 2, select=c(ClaimDay, MeanCost))

par(mfrow=c(2,1))
plot(claimsCost1)
plot(claimsCost2)

claimsCostY1 <- subset(claimsMeanCost1Y, ClaimType == 1, select=c(ClaimDay365, MeanCost))
claimsCostY2 <- subset(claimsMeanCost1Y, ClaimType == 2, select=c(ClaimDay365, MeanCost))

par(mfrow=c(2,1))
plot(claimsCostY1)
plot(claimsCostY2)
