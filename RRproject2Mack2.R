## Risk och reserv Projekt 2

#library(ChainLadder)
#library(xtable)


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

# Chainladder
n <- 10
f <- sapply(1:(n-1),
               function(i){
                 sum(triangle.2[c(1:(n-i)),i+1])/sum(triangle.2[c(1:(n-i)),i])
               }
)

sigma <- sapply(1:(n-1),
            function(i){
              sum(triangle.2[c(1:(n-i)),i]*(triangle.2[c(1:(n-i)),i+1]/triangle.2[c(1:(n-i)),i] - f[i])^2)/(n-i-1)
            }
)





mack.2 <- MackChainLadder(triangle.2, est.sigma="Mack")
mack.2
mack.2$f
mack.2$FullTriangle
plot(mack.2)







