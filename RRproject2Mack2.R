## Risk och reserv Projekt 2

library(ChainLadder)
library(xtable)


claims <- read.table("Projekt2_Grupp8.txt", header = TRUE, sep = ";")
summary(claims)
head(claims, n=10)

sub.2 <- subset(claims, is.element(ClaimType, 2),
                select=c(ClaimDay, PaymentDay, ClaimCost))

sub.2$ClaimYear <- sub.2$ClaimDay %/% 366 + 1
sub.2$PaymentYear <- sub.2$PaymentDay %/% 366 + 1

sub.2.yearly <- 
  aggregate(ClaimCost ~ ClaimYear + PaymentYear, data=sub.2, FUN=sum)
sub.2.yearly$Development <-
  sub.2.yearly$PaymentYear - sub.2.yearly$ClaimYear

# Last 10 years
s.2.yearly <- subset(sub.2.yearly, ClaimYear > 10 & Development < 10)
T.2 <- incr2cum(as.triangle(s.2.yearly,
                                   origin="ClaimYear",
                                   dev="Development",
                                   value="ClaimCost"), na.rm = FALSE)

# Chainladder
n <- 10
f <- sapply(1:(n-1),
               function(i){
                 sum(T.2[c(1:(n-i)),i+1])/sum(T.2[c(1:(n-i)),i])
               }
)

sigma.sq <- sapply(1:(n-1),
            function(i){
              sum(T.2[c(1:(n-i)),i]*(T.2[c(1:(n-i)),i+1]/
                                       T.2[c(1:(n-i)),i] -
                                       f[i])^2)/(n-i-1)
            }
)

# Lazy way
mack.2 <- MackChainLadder(T.2, est.sigma="Mack")
mack.2
mack.2$f
mack.2$FullTriangle
plot(mack.2)



# Check
f
#3.402217 1.521390 1.177938 1.083928 1.042521 1.020895 1.009772 1.005389 1.000891
mack.2$f
#3.402217 1.521390 1.177938 1.083928 1.042521 1.020895 1.009772 1.005389 1.000891 1.000000

sqrt(sigma.sq)
#816.946479 99.178128 129.949537 39.178689 50.198601 33.094542 3.973309 35.430556
mack.2$sigma
#816.946479 99.178128 129.949537 39.178689  50.198601 33.094542 3.973309 35.430556 3.973309


# ITERATION
#First 10 years
s.22.yearly <- subset(sub.2.yearly, ClaimYear < 11 &
                        Development < 10)
T.22 <- incr2cum(as.triangle(s.22.yearly,
                                   origin="ClaimYear",
                                   dev="Development",
                                   value="ClaimCost"),
                                   na.rm = FALSE)

ultimate.claims <- rep(0, 9)
U0 <- matrix(T.22,nrow=10,ncol=10)
for (i in 2:10)
{
  tt <-2
  U <- U0
  for (r in i:10)
  {
    # Upper (left) triangle
    U[r,(10+i-r):10] <- NA
  }
  m <- MackChainLadder(as.triangle(U), est.sigma="Mack")
  ultimate.claims[i-1] <- m$FullTriangle[10,10]
}

t <- 1:9
plot(t,ultimate.claims)
lines(t,rep(U0[10,10],9))





