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


m2 <- max(sub.2.yearly$ClaimYear) + 1
n2 <- max(sub.2.yearly$Development) + 1
T.2 <- matrix(0,m2,n2)
for (m in 1:m2)
{
  for (n in 1:n2)
  {
    c <- sub.2.yearly$ClaimCost[sub.2.yearly$ClaimYear == (m-1) &
                                sub.2.yearly$Development == (n-1)]
    if (length(c)>0)
      T.2[m,n] <- c
    else
      T.2[m,n] <- NA
  }
}

# Cumulative triangle
C.2 <- matrix(NA,m2,n2)
for (m in 1:m2-1)
{
  n.values <- which(!is.na(T.2[m,]))
  n.max <- max(2,n.values)
  C.2[m,1] <- T.2[m,1]
  for (n in 2:n.max)
  {
    if (is.element(n,n.values))
      C.2[m,n] <- C.2[m,n-1] + T.2[m,n]
    else
      C.2[m,n] <- C.2[m,n-1]
  }
}
C.2[m2,1] <- T.2[m2,1]

CC.2 <- C.2[11:nrow(C.2),1:10]

triangle.2 <- as.triangle(CC.2)

triangle.2


mack.2 <- MackChainLadder(triangle.2, est.sigma="Mack")
mack.2
mack.2$f
mack.2$FullTriangle
plot(mack.2)

CC.2[1,10]*10







