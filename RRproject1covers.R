## Risk och reserv Projekt 1

#save(S1,S2, file = "C.RData")
# Generated copula (S1,S2)
load("C.RData")

summary(S1)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 64835115 69340476 70366080 70330397 71315888 75126283 
quantile(S1,0.5)
# 50% 
# 70366080
quantile(S1,c(0.1,0.5,0.9))
v <- (1:100)/100
plot(v,quantile(S1,v))

hist(S1+S2,100)

plot(v,quantile(S1+S2,v))

# SL-cover single
summary(S1+S2)
K <- quantile(S1+S2,0.9)
# 249130402

Rsl <- max(0,S1+S2-K)
price <- 1.1*mean(Rsl)
# 18507884