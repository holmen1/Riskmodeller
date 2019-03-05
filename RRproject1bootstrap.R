## Risk och reserv Projekt 1

### BOOTSTRAP

claims <- read.table("Projekt1_Grupp8.txt", header = TRUE, sep = ";")

claims.daily <- aggregate(list(ClaimCost=claims$ClaimCost),
                          list(ClaimDay=claims$ClaimDay,
                               ClaimType=claims$ClaimType), sum)
claims.daily$ClaimDay365 <- claims.daily$ClaimDay %% 365
claims.daily$ClaimDay365[claims.daily$ClaimDay365==0] <- 365

claims.daily$ClaimYear <- (claims.daily$ClaimDay%/%366) +1
claims.daily$ClaimMonth <- 0

months<-c(0,31,28,31,30,31,30,31,31,30,31,30,31)
months.cum<-cumsum(months)

for (m in 1:(length(months.cum)-1))
  claims.daily$ClaimMonth[months.cum[m] < claims.daily$ClaimDay365 &
                            claims.daily$ClaimDay365 <= months.cum[m+1]]<- m

cost.monthly<-aggregate(list(ClaimCost=claims.daily$ClaimCost),
                        list(ClaimYear=claims.daily$ClaimYear,
                             ClaimMonth=claims.daily$ClaimMonth,
                             ClaimType=claims.daily$ClaimType),
                        sum)

# Showing wintersummer
rho.ws <- cor(cost.monthly$ClaimCost[cost.monthly$ClaimType==1],
              cost.monthly$ClaimCost[cost.monthly$ClaimType==2])
plot(cost.monthly$ClaimCost[cost.monthly$ClaimType==1],
     cost.monthly$ClaimCost[cost.monthly$ClaimType==2],main=paste("rho=",rho.ws))


year <- unique(cost.monthly$ClaimYear)
month <- unique(cost.monthly$ClaimMonth)


tmp<-unique(cost.monthly[c("ClaimYear","ClaimMonth")])
tmp$Cost1 <- cost.monthly$ClaimCost[tmp$ClaimYear==cost.monthly$ClaimYear &
                                      tmp$ClaimMonth==cost.monthly$ClaimMonth &
                                      cost.monthly$ClaimType == 1]
tmp$Cost2 <- cost.monthly$ClaimCost[tmp$ClaimYear==cost.monthly$ClaimYear &
                                      tmp$ClaimMonth==cost.monthly$ClaimMonth &
                                      cost.monthly$ClaimType == 2]

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
rho.bs <- cor(BS[,1],BS[,2]) #= 0.8629129
plot(BS[,1],BS[,2],main=paste("rho=",rho.bs))