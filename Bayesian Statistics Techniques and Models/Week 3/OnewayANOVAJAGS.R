data("PlantGrowth")
?PlantGrowth
PlantGrowth

boxplot(weight~group,data=PlantGrowth)

lmod=lm(weight~group,data=PlantGrowth)
summary(lmod)
anova(lmod)


library("rjags")
##bayesian model in jags
mod_string="model{
  for (i in 1:length(y)){
    y[i]~dnorm(mu[grp[i]],prec)
  }
  
  for (j in 1:3){
    mu[j]~dnorm(0.0,1.0/1.0e6)
  }
  prec~dgamma(5/2.0,5*1.0/2.0)
  sig=sqrt(1.0/prec)
}"

set.seed(82)
str(PlantGrowth)
data_jags=list(y=PlantGrowth$weight,grp=as.numeric(PlantGrowth$group))


params=c("mu","sig")

inits=function(){
  inits=list("mu"=rnorm(3,0.0,100.0),"prec"=rgamma(1,1,1))
}

mod=jags.model(textConnection(mod_string),data=data_jags,inits=inits,
               n.chains=3)

update(mod,1e3)

mod_sim=coda.samples(model=mod,variable.names=params,
                    n.iter=5e3)

mod_csim=as.mcmc(do.call(rbind,mod_sim))
par(mar=c(5,5,2,2))
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

pm_params=colMeans(mod_csim)
pm_params

yhat=pm_params[1:3][data_jags$grp]
yhat

resid=data_jags$y-yhat
plot(resid)
plot(yhat,resid)

summary(mod_sim)
HPDinterval(mod_csim)
HPDinterval(mod_csim,0.9)


head(mod_csim)
mean(mod_csim[,3]>mod_csim[,1])

mean(mod_csim[,3]>1.1*mod_csim[,1])








#QUIZ
library("rjags")
##bayesian model in jags
mod_string="model{
  for (i in 1:length(y)){
    y[i]~dnorm(mu[grp[i]],prec[grp[i]])
  }
  
  for (j in 1:3){
    mu[j]~dnorm(0.0,1.0/1.0e6)
  }
  
  for (i in 1:3){
  
  
  prec[i]~dgamma(5/2.0,5*1.0/2.0)
  sig[i]=sqrt(1.0/prec[i])}
  
  
}"

set.seed(82)
str(PlantGrowth)
data_jags=list(y=PlantGrowth$weight,grp=as.numeric(PlantGrowth$group))


params=c("mu","sig")

inits=function(){
  inits=list("mu"=rnorm(3,0.0,100.0),"prec"=rgamma(3,1,1))
}

mod1=jags.model(textConnection(mod_string),data=data_jags,inits=inits,
               n.chains=3)

update(mod,1e3)

mod_sim=coda.samples(model=mod1,variable.names=params,
                     n.iter=5e3)

mod_csim=as.mcmc(do.call(rbind,mod_sim))
par(mar=c(5,5,2,2))
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

pm_params1=colMeans(mod_csim)
pm_params1

yhat=pm_params1[1:3][data_jags$grp]
yhat

resid=data_jags$y-yhat
plot(resid)
plot(yhat,resid)

summary(mod_sim)
HPDinterval(mod_csim)
HPDinterval(mod_csim,0.9)


head(mod_csim)
mean(mod_csim[,3]>mod_csim[,1])

mean(mod_csim[,3]>1.1*mod_csim[,1])


#Question 4
dic1 = dic.samples(mod, n.iter = 20000)
dic2 = dic.samples(mod1, n.iter = 20000)

print(dic2-dic1)
print(dic1-dic2)
