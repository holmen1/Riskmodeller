## Risk och reserv Projekt 2

#library(ChainLadder)

claims <- read.table("Projekt2_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

sub.2 <- subset(claims, is.element(ClaimType, 2), select=c(ClaimDay, PaymentDay, ClaimCost))

sub.2$ClaimYear <- sub.2$ClaimDay %/% 366
sub.2$PaymentYear <- sub.2$PaymentDay %/% 366

sub.2.yearly <- aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=sub.2, FUN=sum)
sub.2.yearly$Development <- sub.2.yearly$PaymentYear - sub.2.yearly$ClaimYear

#
sub.2.yearly <- subset(sub.2.yearly, ClaimYear > 9)
triangle.2 <- incr2cum(as.triangle(sub.2.yearly,
                                   origin="ClaimYear",
                                   dev="Development",
                                   value="ClaimCost"), na.rm = FALSE)

mack.2 <- MackChainLadder(triangle.2, est.sigma="Mack")
mack.2
mack.2$f
mack.2$FullTriangle
plot(mack.2)







