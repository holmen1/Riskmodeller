## Risk och reserv Projekt 1

### BOOTSTRAP

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")

# Olika värdedagar claim1 ? claim2 = inner join
CommonDays<-intersect(claims$ClaimDay[claims$ClaimType==1],
                      claims$ClaimDay[claims$ClaimType==2])

claims.daily <- aggregate(list(ClaimCost=claims$ClaimCost),list(ClaimDay=claims$ClaimDay, ClaimType=claims$ClaimType), sum)
claims.daily$ClaimDay365 <- claims.daily$ClaimDay %% 365
claims.daily$ClaimDay365[claims.daily$ClaimDay365==0] <- 365


claims.daily$ClaimYear <- (claims.daily$ClaimDay%/%366) +1
claims.daily$ClaimMonth <- 0
months<-c(0,31,28,31,30,31,30,31,31,30,31,30,31)
months.cum<-cumsum(months)

for (m in 1:(length(months.cum)-1))
  claims.daily$ClaimMonth[months.cum[m] < claims.daily$ClaimDay365 & claims.daily$ClaimDay365 <= months.cum[m+1]]<- m

cost.monthly<-aggregate(list(ClaimCost=claims.daily$ClaimCost),list(ClaimYear=claims.daily$ClaimYear, ClaimMonth=claims.daily$ClaimMonth, ClaimType=claims.daily$ClaimType), sum)


# Showing wintersummer
plot(cost.monthly$ClaimCost[cost.monthly$ClaimType==1],cost.monthly$ClaimCost[cost.monthly$ClaimType==2])

year <- unique(cost.monthly$ClaimYear)
month <- unique(cost.monthly$ClaimMonth)


tmp<-unique(cost.monthly[c("ClaimYear","ClaimMonth")])
tmp$Cost1 <- cost.monthly$ClaimCost[tmp$ClaimYear==cost.monthly$ClaimYear & tmp$ClaimMonth==cost.monthly$ClaimMonth & cost.monthly$ClaimType == 1]
tmp$Cost2 <- cost.monthly$ClaimCost[tmp$ClaimYear==cost.monthly$ClaimYear & tmp$ClaimMonth==cost.monthly$ClaimMonth & cost.monthly$ClaimType == 2]

winter.months<-c(1,2,3,4,9,10,11,12)
summer.months<-c(5,6,7)
# Winter
cost.winter<-subset(tmp, is.element(tmp$ClaimMonth, winter.months), 
                    select=c(Cost1,Cost2))
# Summer
cost.summer<-subset(tmp, is.element(tmp$ClaimMonth, summer.months), 
                    select=c(Cost1,Cost2))

n <- 1000
BS <- matrix(0,n,2)
for (k in 1:n)
{
  s.W<-tmp[sample(nrow(cost.winter),8, replace=TRUE),]
  s.S<-tmp[sample(nrow(cost.summer),4, replace=TRUE),]
  BS[k,1]<-sum(s.W$Cost1) + sum(s.S$Cost1)
  BS[k,2]<-sum(s.W$Cost2) + sum(s.S$Cost2)
}
rho.bs <- cor(BS[,1],BS[,2]) #= 0.5277435
plot(BS[,1],BS[,2],main=paste("rho=",rho.bs))

plot(BS.seasonal[,1],BS.seasonal[,2])
cor(BS.seasonal[,1],BS.seasonal[,2])





cost.by.year<-aggregate(list(Cost=cost.by.month$Cost),list(Year=cost.by.month$Year, ClaimType=cost.by.month$ClaimType), sum)


plot(cost.by.year$Cost[cost.by.year$ClaimType==1],cost.by.year$Cost[cost.by.year$ClaimType==2])
cor(cost.by.year$Cost[cost.by.year$ClaimType==1],cost.by.year$Cost[cost.by.year$ClaimType==2])

# s1<-subset(cost.by.year, cost.by.year$ClaimType==1,select=c(Year,Cost))
# s2<-subset(cost.by.year, cost.by.year$ClaimType==2,select=c(Year,Cost))
# Y<-matrix(0,10,2)
# for (k in 1:10)
# {
#   Y[k,1]<-s1$Cost[s1$Year==(k-1)]
#   Y[k,2]<-s2$Cost[s2$Year==(k-1)]
# }
# plot(Y[,1],Y[,2])
# cor(Y[,1],Y[,2])


# Kontroll
#sum(claims.daily$Cost)==sum(cost.by.month$Cost)



tmp<-unique(cost.by.month[c("Year","Month")])
tmp$Cost1<-cost.by.month$Cost[tmp$Year==cost.by.month$Year & tmp$Month==cost.by.month$Month & cost.by.month$ClaimType == 1]
tmp$Cost2<-cost.by.month$Cost[tmp$Year==cost.by.month$Year & tmp$Month==cost.by.month$Month & cost.by.month$ClaimType == 2]

bs.samples<-1000
# BS<-matrix(0,bs.samples,2)
# for (k in 1:bs.samples)
# {
#   s<-tmp[sample(nrow(tmp),12, replace=TRUE),]
#   BS[k,1]<-sum(s$Cost1)
#   BS[k,2]<-sum(s$Cost2)
# }
# plot(BS[,1],BS[,2])
# cor(BS[,1],BS[,2])

winter.months<-c(1,2,3,4,9,10,11,12)
summer.months<-c(5,6,7)
# Winter
cost.winter<-subset(tmp, is.element(tmp$Month, winter.months), 
       select=c(Cost1,Cost2))
# Summer
cost.summer<-subset(tmp, is.element(tmp$Month, summer.months), 
                    select=c(Cost1,Cost2))

BS.seasonal<-matrix(0,bs.samples,2)
for (k in 1:bs.samples)
{
  s.W<-tmp[sample(nrow(cost.winter),8, replace=TRUE),]
  s.S<-tmp[sample(nrow(cost.summer),4, replace=TRUE),]
  BS.seasonal[k,1]<-sum(s.W$Cost1) + sum(s.S$Cost1)
  BS.seasonal[k,2]<-sum(s.W$Cost2) + sum(s.S$Cost2)
}
rhoY<-cor(Y[,1],Y[,2]) #= 0.5277435
plot(Y[,1],Y[,2],main=paste("rho=",rhoY))

plot(BS.seasonal[,1],BS.seasonal[,2])
cor(BS.seasonal[,1],BS.seasonal[,2])


# ny kula

# Olika värdedagar claim1 ? claim2 = inner join
CommonDays<-intersect(claims.daily$ClaimDay[claims.daily$ClaimType==1],
                      claims.daily$ClaimDay[claims.daily$ClaimType==2])


Y<-matrix(0,length(CommonDays),2)
for (k in 1:length(CommonDays))
{
  #print(k)
  Y[k,1] <- claims.daily$Cost[claims.daily$ClaimDay==CommonDays[k] & claims.daily$ClaimType==1]
  Y[k,2] <- claims.daily$Cost[claims.daily$ClaimDay==CommonDays[k] & claims.daily$ClaimType==2]
}

CommonDays<-intersect(claims.daily$ClaimDay[claims.daily$ClaimType==1],
                      claims.daily$ClaimDay[claims.daily$ClaimType==2])

#cor(claims.daily$Cost[claims.daily$ClaimType==1],claims.daily$Cost[claims.daily$ClaimType==2])






