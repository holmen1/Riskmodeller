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

claims.yearly <- aggregate(ClaimCost ~ ClaimType + ClaimYear + PaymentYear, data=claims, FUN=sum)
claims.yearly$Development <- claims.yearly$PaymentYear - claims.yearly$ClaimYear


# sub.1 <- subset(claims.yearly, is.element(ClaimType, 1) & ClaimYear > 9)
# triangle.1 <- incr2cum(as.triangle(sub.1,
#                                     origin="ClaimYear",
#                                     dev="Development",
#                                     value="ClaimCost"), na.rm = FALSE)

sub.2 <- subset(claims.yearly, is.element(ClaimType, 2) & ClaimYear > 9)
triangle.2 <- incr2cum(as.triangle(sub.2,
                                   origin="ClaimYear",
                                   dev="Development",
                                   value="ClaimCost"), na.rm = FALSE)

#plot(triangle.1)
plot(triangle.2)

#triangle.1
triangle.2

#plot(triangle.1, lattice=TRUE)
plot(triangle.2, lattice=TRUE)

# mack.1 <- MackChainLadder(triangle.1, est.sigma="Mack")
# mack.1
# mack.1$f
# mack.1$FullTriangle
# plot(mack.1)

mack.2 <- MackChainLadder(triangle.2, est.sigma="Mack")
mack.2
mack.2$f
mack.2$FullTriangle
plot(mack.2)






