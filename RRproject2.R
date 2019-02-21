## Risk och reserv Projekt 2

library(MASS)

claims <- read.table("Projekt2_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

# Label to days in 1 Y 1-365
claims$ClaimDay365 <- claims$ClaimDay %% 365
claims$PaymentDay365 <- claims$PaymentDay %% 365
claims$ClaimDay365[claims$ClaimDay365==0] <- 365
claims$PaymentDay365[claims$PaymentDay365==0] <- 365

claims$ClaimYear <- claims$ClaimDay %/% 366
claims$PaymentYear <- claims$PaymentDay %/% 366




summary(claims)
head(claims, n=1000)

claims.1 <- subset(claims,
                   ClaimType == 1,
                   select=c(ClaimDay,ClaimDay365,ClaimYear,PaymentDay,PaymentDay365,PaymentYear,ClaimCost))
claims.2 <- subset(claims,
                   ClaimType == 2,
                   select=c(ClaimDay,ClaimDay365,ClaimYear,PaymentDay,PaymentDay365,PaymentYear,ClaimCost))

claims.1.yearly <- aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=claims.1, FUN=sum)
claims.2.yearly <- aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=claims.1, FUN=sum)

write.csv(claims.1.yearly, file = "claims1Y.csv")
write.csv(claims.2.yearly, file = "claims2Y.csv")






cost.daily <- aggregate(list(Cost=claims$ClaimCost),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)




claims.365<-subset(claims, select=c("ClaimType","ClaimDay"))



arrivals.daily <- aggregate(list(Arrivals=claims$ClaimDay),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), length)
cost.daily <- aggregate(list(Cost=claims$ClaimCost),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)

