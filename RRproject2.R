## Risk och reserv Projekt 2

#library(MASS)
library(ChainLadder)

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
claims.2.yearly <- aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=claims.2, FUN=sum)

#write.csv(claims.1.yearly, file = "claims1Y.csv")
#write.csv(claims.2.yearly, file = "claims2Y.csv")

claims.1.yearly$Development <- claims.1.yearly$PaymentYear - claims.1.yearly$ClaimYear
claims.2.yearly$Development <- claims.2.yearly$PaymentYear - claims.2.yearly$ClaimYear

# Claim triangles 1 & 2
m1 <- max(claims.1.yearly$ClaimYear) + 1
n1 <- max(claims.1.yearly$Development) + 1
T.1 <- matrix(0,m1,n1)
for (m in 1:m1)
{
  for (n in 1:n1)
  {
    c <- claims.1.yearly$ClaimCost[claims.1.yearly$ClaimYear == (m-1) &
                                   claims.1.yearly$Development == (n-1)]
    T.1[m,n] <- max(0,c)
  }
}
triangle.1 <- incr2cum(as.triangle(T.1))

m2 <- max(claims.2.yearly$ClaimYear) + 1
n2 <- max(claims.2.yearly$Development) + 1
T.2 <- matrix(0,m2,n2)
for (m in 1:m2)
{
  for (n in 1:n2)
  {
    c <- claims.2.yearly$ClaimCost[claims.2.yearly$ClaimYear == (m-1) &
                                   claims.2.yearly$Development == (n-1)]
    T.2[m,n] <- max(0,c)
  }
}
triangle.2 <- incr2cum(as.triangle(T.2))


plot(triangle.1)
plot(triangle.2)
#plot(triangle.1, lattice=TRUE)





