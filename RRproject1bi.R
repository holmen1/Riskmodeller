## Risk och reserv Projekt 1

library(MASS)

load("fitted.RData")
head(claims.daily,n=20)

length(claims.daily$Cost[claims.daily$ClaimType==1])
length(claims.daily$Cost[claims.daily$ClaimType==2])

CommonDays<-intersect(claims.daily$ClaimDay[claims.daily$ClaimType==1],
                      claims.daily$ClaimDay[claims.daily$ClaimType==2])

cost.common <- subset(claims.daily, claims.daily$ClaimDay %in% CommonDays, 
                  select=c(ClaimDay365, ClaimType, Cost))
cost.year<-aggregate(list(Cost=cost.common$Cost),list(ClaimDay365=cost.common$ClaimDay365, ClaimType=cost.common$ClaimType), sum)

head(cost.year,n=20)
plot(cost.year$Cost[cost.year$ClaimType==1],
     cost.year$Cost[cost.year$ClaimType==2])
cor(cost.year$Cost[cost.year$ClaimType==1],
     cost.year$Cost[cost.year$ClaimType==2])

# Kontroll
sum(cost.year$Cost[cost.year$ClaimType==1])
sum(cost.year$Cost[cost.year$ClaimType==2])
sum(claims.daily$Cost[claims.daily$ClaimType==1])
sum(claims.daily$Cost[claims.daily$ClaimType==2])

plot(cost.common$Cost[cost.common$ClaimType==1],
     cost.common$Cost[cost.common$ClaimType==2])


cor(cost.common$Cost[cost.common$ClaimType==1],
     cost.common$Cost[cost.common$ClaimType==2])





