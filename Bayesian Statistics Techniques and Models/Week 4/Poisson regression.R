library("COUNT")
data("badhealth")
?badhealth
head(badhealth)
any(is.na(badhealth))

hist(badhealth$numvisit,breaks=20)
min(badhealth$numvisit)
sum(badhealth$numvisit==0)

plot(jitter(log(numvisit))~jitter(age),data=badhealth,subset=badh==0&numvisit>0,
     xlab="age",ylab="log(visits")

points(jitter(log(numvisit))~jitter(age),data=badhealth,subset=badh==1&numvisit>0,
     xlab="age",ylab="log(visits",col="red")
 
str(badhealth)



##Poisson linear model
library("rjags")
mod_string="model{
  for (i in 1:length(numvisit)){
    numvisit[i] ~ dpois(lam[i])
    log(lam[i])=int+b_badh*badh[i]+b_age*age[i]+b_intx*age[i]*badh[i]
  }
  
  int~dnorm(0,1/1e6)
  b_badh~dnorm(0,1/1e4)
  b_age~dnorm(0,1/1e4)
  b_intx~dnorm(0.0,1.0/1e4)
}"





set.seed(102)
data_jags=as.list(badhealth)
params=c("int","b_badh","b_age","b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod,1e3)

mod_sim=coda.samples(model=mod,variable.names=params,n.iter=5e3)

mod_csim=as.mcmc(do.call(rbind,mod_sim))


##Convergence diagnostic
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)


##compute DIC

dic = dic.samples(mod, n.iter=1e3)
dic

##Residuals
X = as.matrix(badhealth[,-1])
X = cbind(X, with(badhealth, badh*age))
head(X)

(pmed_coef = apply(mod_csim, 2, median))

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)

hist(lam_hat)


resid = badhealth$numvisit - lam_hat
plot(resid) # the data were ordered


plot(lam_hat, badhealth$numvisit)
abline(0.0, 1.0)


plot(lam_hat[which(badhealth$badh==0)], resid[which(badhealth$badh==0)], xlim=c(0, 8), 
     ylab="residuals", xlab=expression(hat(lambda)), ylim=range(resid))

points(lam_hat[which(badhealth$badh==1)], resid[which(badhealth$badh==1)], col="red")


var(resid[which(badhealth$badh==0)])

var(resid[which(badhealth$badh==1)])


##Predictive Distribution
summary(mod_sim)


dat=read.table(file="C:/Users/dziny/Downloads/Bayesian Statistics Coursera/Bayesian Statistics Techniques and Models/Week 4/cookies.dat.txt",header=TRUE)

dat1=read.table(file="cookies.dat.txt",header=TRUE)
dat1


x1 = c(0, 35, 0) # good health
x2 = c(1, 35, 35) # bad health

head(mod_csim)

loglam1 = mod_csim[,"int"] + mod_csim[,c(2,1,3)] %*% x1
loglam2 = mod_csim[,"int"] + mod_csim[,c(2,1,3)] %*% x2

lam1 = exp(loglam1)
lam2 = exp(loglam2)


head(loglam1)

lam1= exp(loglam1)
lam2= exp(loglam2)

plot(density(lam1))



n.sim=length(lam1)
n.sim


y1=rpois(n_sim,lam1)
y2=rpois(n_sim,lam2)


plot(table(factor(y1, levels=0:18))/n_sim, pch=2, ylab="posterior prob.", xlab="visits")
points(table(y2+0.1)/n_sim, col="red")

mean(y2>y1)



#Quiz llesson 10
#Question 1
yi=1.5-0.3*(0.8)+1*(1.2)
exp(yi)


#Question 2
##Poisson linear model
library("rjags")
mod_string="model{
  for (i in 1:length(numvisit)){
    numvisit[i] ~ dpois(lam[i])
    log(lam[i])=int+b_badh*badh[i]+b_age*age[i]
  }
  
  int~dnorm(0,1/1e6)
  b_badh~dnorm(0,1/1e4)
  b_age~dnorm(0,1/1e4)
  b_intx~dnorm(0.0,1.0/1e4)
}"

#Question 4
sum(dpois(0:21, lambda = 30))


#Question 5
dat=read.csv(file="callers.csv",header=TRUE)
dat

pairs(dat)


#Question 7
library("rjags")
mod_string="model{
  for (i in 1:length(calls)){
    calls[i] ~ dpois(lam[i]*days_active[i])
    log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
  }
  
  b0~dnorm(0,1/1e2)
  b1~dnorm(0,1/1e2)
  b2~dnorm(0,1/1e2)
  
}"

set.seed(102)
data_jags=as.list(dat)
params=c("b0","b1","b2")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod,1e3)

mod_sim=coda.samples(model=mod,variable.names=params,n.iter=15e3)

mod_csim=as.mcmc(do.call(rbind,mod_sim))


##Convergence diagnostic
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)


##compute DIC

dic = dic.samples(mod, n.iter=1e3)
dic

head(mod_csim)
mean(mod_csim[,3] > 0)
























mod3_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i]*days_active[i])
    log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  }
  
  b0~dnorm(0.0, 1.0/1e2)
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1e2)
  }
} "

data3_jags = as.list(dat)

params3 = c("b0", "b")


mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=15e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

## convergence diagnostics
plot(mod3_sim)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)
effectiveSize(mod3_sim)

## compute DIC
dic3 = dic.samples(mod3, n.iter=1e3)

pmod3_coef = apply(mod3_csim, 2, mean)

X = as.matrix(dat[,-c(1,2)])
X = X[,c(2,1)]

llam_hat3 = pmod3_coef['b0'] + X %*% pmod3_coef[-3] #delete the third column
lam_hat3 = llam_hat3*dat$days_active
#posterior probability that beta the coefficient is greater than 0?
# beta parameter value is in second column of mod_csim:
mean(mod3_csim[,2] > 0)
