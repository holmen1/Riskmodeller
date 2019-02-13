## Risk och reserv Projekt 1

### BOOTSTRAP

library(MASS)
library(psych)

load("fitted.RData")


claims.daily$Year <- claims.daily$ClaimDay%/%366
claims.daily$Month <- 0
months<-c(0,31,28,31,30,31,30,31,31,30,31,30,31)
months.cum<-cumsum(months)
for (m in 1:(length(months.cum)-1))
  claims.daily$Month[months.cum[m] < claims.daily$ClaimDay365 & claims.daily$ClaimDay365 <= months.cum[m+1]]<- m

cost.by.month<-aggregate(list(Cost=claims.daily$Cost),list(Year=claims.daily$Year, Month=claims.daily$Month, ClaimType=claims.daily$ClaimType), sum)

# Kontroll
#sum(claims.daily$Cost)==sum(cost.by.month$Cost)

plot(cost.by.month$Cost[cost.by.month$ClaimType==1],cost.by.month$Cost[cost.by.month$ClaimType==2])

tmp1 <- subset(cost.by.month, ClaimType == 1, select=c(Year,Month,Cost))
tmp2 <- subset(cost.by.month, ClaimType == 2, select=c(Year,Month,Cost))

tmp<-unique(cost.by.month[c("Year","Month")])
tmp$Cost1<-cost.by.month$Cost[tmp$Year==cost.by.month$Year & tmp$Month==cost.by.month$Month & cost.by.month$ClaimType == 1]
tmp$Cost2<-cost.by.month$Cost[tmp$Year==cost.by.month$Year & tmp$Month==cost.by.month$Month & cost.by.month$ClaimType == 2]


bs.samples<-1000
BS<-matrix(0,bs.samples,2)
for (k in 1:bs.samples)
{
  sample<-tmp[sample(nrow(tmp),12, replace=TRUE),]
  BS[k,1]<-sum(sample$Cost1)
  BS[k,2]<-sum(sample$Cost2)
}

plot(BS[,1],BS[,2])
cor(BS[,1],BS[,2])





