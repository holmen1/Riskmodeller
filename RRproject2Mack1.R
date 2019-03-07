## Risk och reserv Projekt 2

#library(ChainLadder)

claims <- read.table("Projekt2_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

sub.1 <- subset(claims, is.element(ClaimType, 1),
                select=c(ClaimDay, PaymentDay, ClaimCost))

sub.1$ClaimYear <- sub.1$ClaimDay %/% 366
sub.1$PaymentYear <- sub.1$PaymentDay %/% 366

sub.1.yearly <-
  aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=sub.1, FUN=sum)
sub.1.yearly$Development <-
  sub.1.yearly$PaymentYear - sub.1.yearly$ClaimYear


m1 <- max(sub.1.yearly$ClaimYear) + 1
n1 <- max(sub.1.yearly$Development) + 1
T.1 <- matrix(0,m1,n1)
for (m in 1:m1)
{
  for (n in 1:n1)
  {
    c <- sub.1.yearly$ClaimCost[
                      sub.1.yearly$ClaimYear == (m-1) &
                      sub.1.yearly$Development == (n-1)]
    if (length(c)>0)
      T.1[m,n] <- c
    else
      T.1[m,n] <- NA
  }
}

T.1[11:nrow(T.1),1:8]

# Cumulative triangle
C.1 <- matrix(NA,m1,n1)
for (m in 1:m1-1)
{
  n.values <- which(!is.na(T.1[m,]))
  n.max <- max(2,n.values)
  C.1[m,1] <- T.1[m,1]
  for (n in 2:n.max)
  {
    if (is.element(n,n.values))
      C.1[m,n] <- C.1[m,n-1] + T.1[m,n]
    else
      C.1[m,n] <- C.1[m,n-1]
  }
}
C.1[m1,1] <- T.1[m1,1]

CC.1 <- C.1[11:nrow(C.1),1:8]

triangle.1 <- as.triangle(CC.1)

mack.1 <- MackChainLadder(triangle.1, est.sigma="Mack")
mack.1
mack.1$f
mack.1$FullTriangle
plot(mack.1)

# Totals
# Latest:   108,727,807.00
# Dev:                0.95
# Ultimate: 114,258,977.85
# IBNR:       5,531,170.85
# Mack.S.E      529,550.67
# CV(IBNR):           0.10





