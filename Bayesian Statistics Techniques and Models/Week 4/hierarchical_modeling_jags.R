dat=read.table(file="cookies.dat",header=TRUE)

dat

library("rjags")

mod_string= "model{
  for (i in 1:length(chips)){
    chips[i]~dpois(lam[location[i]])
  }
  for (j in 1:max(location)){
    lam[j]~dgamma(alpha,beta)
  }
  mu~dgamma(2.0,1/5)
  sig~dexp(1)
  
  alpha=mu^2/sig^2
  beta=mu/sig^2
}"

set.seed(113)
data_jags=as.list(dat)
params=c("lam","mu","sig")

mod= jags.model(textConnection(mod_string),data=data_jags,n.chains=3)
update(mod,1e3)

mod_sim=coda.samples(model=mod,variable.names=params,n.iter=5e3)
mod_csim=as.mcmc(do.call(rbind,mod_sim))
par(mar = c(2, 2, 2, 2))
plot(mod_sim,ask=TRUE)

dic=dic.samples(mod,n.iter=1e3)


##Model checking
pm_params=colMeans(mod_csim)

yhat=rep(pm_params[1:5],each=30)
resid=dat$chips-yhat

plot(resid)
plot(jitter(yhat),resid)

#lambda residual
lam_resid=pm_params[1:5]-pm_params["mu"]
plot(lam_resid)
abline(h=0,lty=2)

summary(mod_sim)


##Posterior Predictive Simulation
n_sim=nrow(mod_csim)
post_alpha = mod_csim[,"mu"]^2/mod_csim[,"sig"]^2
post_beta=mod_csim[,"mu"]/mod_csim[,"sig"]^2

lam_pred=rgamma(n_sim,shape=post_alpha,rate=post_beta)
hist(lam_pred)

y_pred=rpois(n_sim,lambda=lam_pred)
hist(y_pred)

hist(dat$chips)
y_pred1=rpois(n_sim,lambda=mod_csim[,"lam[1]"])
hist(y_pred1)



#Quiz
# Q3
dat = read.csv(file="pctgrowth.csv", header=TRUE)
head(dat)

# Q4
#a model that assumes no hierarchy (the ANOVA cell means model). 
#We can approximate the posterior estimates for the five industries: group 1 to 5 
#means under a noninformative prior by simply calculating the sample mean growth for the five industries. 
library(rjags)
mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(theta[grp[i]], prec)
  }
  
  for (j in 1:max(grp)) {
    theta[j] ~ dnorm(mu, tau_sq)
  }
  
  mu ~ dnorm(0, 1/1e6)
  tau_sq ~ dgamma(1.0/2.0, 1.0*3.0/2.0)
  prec ~ dgamma(2.0/2.0, 2*1/2)
  sig = sqrt(1/prec)
} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta", "mu", "sig")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

pm_params = apply(mod_csim, 2, mean)
means_theta = pm_params[-c(1,2)]
means_theta

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
## dat is the data read from pctgrowth.csv

plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.


