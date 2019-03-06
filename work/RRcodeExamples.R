## Risk och reserv exempel

# Risk and Reserving: Example 1.2

gamma_fun<-function(a,p)
{
	t_val<-(a-1)^2*(a-2)
	n_val<-2*p*(a-1)-p^2*(a-2)
	sqrt(t_val/n_val)
}

p<-0.05
a_vec<-c(2.5,10)
g_vec<-c(2.5,10)
n_vec<-c(100,1000,10000)

for(i in (1:length(a_vec)))
{
	g_vec[i]<-gamma_fun(a_vec[i],p)
}

runs<-10000
res_mat<-matrix(0,runs,6)
for(i in (1:runs))
{
	unif_vec1<-runif(10000)
	unif_vec2<-runif(10000)

	res_mat[i,1]<-sum(p*g_vec[1]/(a_vec[1]-1)+qnorm(0.99)/sqrt(n_vec[1])-as.double(unif_vec1[1:n_vec[1]]<p)*g_vec[1]*((1-unif_vec2[1:n_vec[1]])^(-1/a_vec[1])-1))	
	res_mat[i,2]<-sum(p*g_vec[2]/(a_vec[2]-1)+qnorm(0.99)/sqrt(n_vec[1])-as.double(unif_vec1[1:n_vec[1]]<p)*g_vec[2]*((1-unif_vec2[1:n_vec[1]])^(-1/a_vec[2])-1))	
	res_mat[i,3]<-sum(p*g_vec[1]/(a_vec[1]-1)+qnorm(0.99)/sqrt(n_vec[2])-as.double(unif_vec1[1:n_vec[2]]<p)*g_vec[1]*((1-unif_vec2[1:n_vec[2]])^(-1/a_vec[1])-1))	
	res_mat[i,4]<-sum(p*g_vec[2]/(a_vec[2]-1)+qnorm(0.99)/sqrt(n_vec[2])-as.double(unif_vec1[1:n_vec[2]]<p)*g_vec[2]*((1-unif_vec2[1:n_vec[2]])^(-1/a_vec[2])-1))	
	res_mat[i,5]<-sum(p*g_vec[1]/(a_vec[1]-1)+qnorm(0.99)/sqrt(n_vec[3])-as.double(unif_vec1[1:n_vec[3]]<p)*g_vec[1]*((1-unif_vec2[1:n_vec[3]])^(-1/a_vec[1])-1))	
	res_mat[i,6]<-sum(p*g_vec[2]/(a_vec[2]-1)+qnorm(0.99)/sqrt(n_vec[3])-as.double(unif_vec1[1:n_vec[3]]<p)*g_vec[2]*((1-unif_vec2[1:n_vec[3]])^(-1/a_vec[2])-1))			
}

par(mfrow=c(3,2))
hist(res_mat[,1],75,xlab="",ylab="",main="alpha=2.5, n=100",col="black")
hist(res_mat[,2],75,xlab="",ylab="",main="alpha=10, n=100",col="black")
hist(res_mat[,3],75,xlab="",ylab="",main="alpha=2.5, n=1000",col="black")
hist(res_mat[,4],75,xlab="",ylab="",main="alpha=10, n=1000",col="black")
hist(res_mat[,5],75,xlab="",ylab="",main="alpha=2.5, n=10000",col="black")
hist(res_mat[,6],75,xlab="",ylab="",main="alpha=10, n=10000",col="black")


colSums(res_mat<0)/runs

# Risk and Reserving: Example 1.3

n_val<-1000

runs<-10000
res_mat2<-matrix(0,runs,2)
for(i in (1:runs))
{
	z_val<-rnorm(1)
	z_vec<-rnorm(n_val)
	res_mat2[i,1]<-sum(exp(z_vec))
	res_mat2[i,2]<-sum(exp(0.1*z_val+sqrt(1-0.1^2)*z_vec))
}

par(mfrow=c(1,2))
hist(res_mat2[,1],75,xlab="",ylab="",main="rho=0",xlim=c(min(res_mat2),max(res_mat2)),col="black")
hist(res_mat2[,2],75,xlab="",ylab="",main="rho=0.1",xlim=c(min(res_mat2),max(res_mat2)),col="black")

# Risk and Reserving: Example 1.4

n_val<-10000
p<-0.05
runs<-10000
cor_vec<-c(0.01,0.02,0.05)
res_mat3<-matrix(0,runs,4)
res_mat3[,1]<-rbinom(runs,n_val,p)
for(i in (1:runs))
{
	res_mat3[i,2]<-rbinom(1,n_val,rbeta(1,(1-cor_vec[1])/cor_vec[1]*p,(1-cor_vec[1])/cor_vec[1]*(1-p)))
	res_mat3[i,3]<-rbinom(1,n_val,rbeta(1,(1-cor_vec[2])/cor_vec[2]*p,(1-cor_vec[2])/cor_vec[2]*(1-p)))
	res_mat3[i,4]<-rbinom(1,n_val,rbeta(1,(1-cor_vec[3])/cor_vec[3]*p,(1-cor_vec[3])/cor_vec[3]*(1-p)))
}

par(mfrow=c(2,2))
hist(res_mat3[,1],75,xlab="",ylab="",main="cor=0",xlim=c(min(res_mat3),max(res_mat3)),col="black")
hist(res_mat3[,2],75,xlab="",ylab="",main="cor=0.01",xlim=c(min(res_mat3),max(res_mat3)),col="black")
hist(res_mat3[,3],75,xlab="",ylab="",main="cor=0.02",xlim=c(min(res_mat3),max(res_mat3)),col="black")
hist(res_mat3[,4],75,xlab="",ylab="",main="cor=0.05",xlim=c(min(res_mat3),max(res_mat3)),col="black")

# Risk and Reserving: Example 1.5

alpha_val<-3
gamma_vec1<-rgamma(10000,shape=50,scale=1/(alpha_val*50))
gamma_vec2<-rgamma(10000,shape=500,scale=1/(alpha_val*500))
par(mfrow=c(2,2))
hist(gamma_vec1,75,xlab="",ylab="",main="1/alpha_est, alpha=3, n=50",xlim=c(min(append(gamma_vec1,gamma_vec2)),max(append(gamma_vec1,gamma_vec2))),col="black")
hist(gamma_vec2,75,xlab="",ylab="",main="1/alpha_est, alpha=3, n=500",xlim=c(min(append(gamma_vec1,gamma_vec2)),max(append(gamma_vec1,gamma_vec2))),col="black")
hist((1-0.99)^(-gamma_vec1),75,xlab="",ylab="",main="quantile_est, n=50",xlim=c(min((1-0.99)^(-append(gamma_vec1,gamma_vec2))),max((1-0.99)^(-append(gamma_vec1,gamma_vec2)))),col="black")
hist((1-0.99)^(-gamma_vec2),75,xlab="",ylab="",main="quantile_est, n=500",xlim=c(min((1-0.99)^(-append(gamma_vec1,gamma_vec2))),max((1-0.99)^(-append(gamma_vec1,gamma_vec2)))),col="black")

# Risk and Reserving: Example 1.6

lossvals<-c(68515,23654,21999,19593,14115,13339,10704,8840,8599,7650,7413,7223,6097,5659,5650,5066,5031,4492,4220,4174,3937,3614,3515,3508,3411,3365,2989,2818,2662,2589,2577,2488,2443,2404,2372,2365,2282,2255,2217,2196)

plot(lossvals,xlab="",ylab="")

H1<-function(g,n,s,xvals)
{
	n*sum(log((g+xvals)/(g+s)))^(-1)
}

H2<-function(g,n,s,xvals)
{
	tmpval1<-sum((g+s)/(g+xvals))
	tmpval2<-sum((xvals-s)/(g+xvals))
	tmpval1/tmpval2
}

xvals<-lossvals[1:39]
s<-lossvals[40]
n<-39
H3<-function(g)
{
	tval<-H1(g,n,s,xvals)
	nval<-H2(g,n,s,xvals)
	tval/nval-1
}

gammahat<-uniroot(H3,lower=0.1,upper=10000)$root
alphahat<-H1(gammahat,n,s,xvals)

plot(sort(xvals,dec=F),gammahat*((1-(1:n)/(n+1))^(-1/alphahat)-1),xlab="data",ylab="model",main="mlfit")

xvals_sort<-sort(xvals,dec=F)
lsfunc<-function(parvec)
{
	sum((xvals_sort-parvec[2]*((1-(1:n)/(n+1))^(-1/parvec[1])-1))^2)
}

estvec_ls<-optim(c(2,200),lsfunc)$par

plot(sort(xvals,dec=F),estvec_ls[2]*((1-(1:n)/(n+1))^(-1/estvec_ls[1])-1),xlab="data",ylab="model",main="lsfit")


truncpardens<-function(x,a,g,s)
{
	(a/g)*(1+x/g)^(-a-1)/((1+s/g)^(-a))
}

library(MASS)

plot(sort(xvals,dec=F),estvec_ml[2]*((1-(1:n)/(n+1))^(-1/estvec_ml[1])-1),xlab="data",ylab="model",main="mlfit 2")
estvec_ml<-fitdistr(lossvals[1:39],truncpardens,start=list(a=2,g=3000),s=lossvals[1],lower=c(1,100))$estimate

estvec_ls

estvec_ml

