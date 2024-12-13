set.seed(72)
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

mod=lm(Anscombe$education~Anscombe$income+Anscombe$young+Anscombe$urban)
summary(mod)
plot(mod)


library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

#data_jags = as.list(Anscombe)



data1_jags=list(education=Anscombe$education,income = Anscombe$income,
                young = Anscombe$young,
                urban = Anscombe$urban)

params1=c("b","sig")

inits1=function(){
  inits=list("b"=rnorm(3.0,0.0,100),"prec"=rgamma(1,1.0,1.0))
}

mod1=jags.model(textConnection(mod_string),data=data1_jags,inits=inits1,n.chains=3)

update(mod1,1000)

mod1_sim=coda.samples(model=mod1,variable.names = params1,n.iter = 5e3)

mod1_csim=do.call(rbind,mod1_sim)


#Convergenge diagnostistics
par(mar=c(5,5,2,2))
plot(mod1_sim)

#gelman and reuben diagnostics
gelman.diag(mod1_sim)

autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

summary(mod1_sim)

summary(mod)

plot(mod)
# here mod_lm is the object saved when you run lm()


##Quiz 7 part b
dic.samples(mod1, n.iter=1e5)

##Drop Urban variable
library("rjags")


mod2_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data2_jags=list(education=Anscombe$education,income = Anscombe$income,
                young = Anscombe$young)

params2=c("b","sig")

inits2=function(){
  inits=list("b"=rnorm(2.0,0.0,100),"prec"=rgamma(1,1.0,1.0))
}

mod2=jags.model(textConnection(mod2_string),data=data2_jags,inits=inits2,n.chains=3)
dic.samples(mod2, n.iter=1e5)




##multiply income and youth

library("rjags")


mod3_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]+b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data3_jags=list(education=Anscombe$education,income = Anscombe$income,
                young = Anscombe$young)

params3=c("b","sig")

inits3=function(){
  inits=list("b"=rnorm(3.0,0.0,100),"prec"=rgamma(1,1.0,1.0))
}

mod3=jags.model(textConnection(mod3_string),data=data3_jags,inits=inits3,n.chains=3)
dic.samples(mod3, n.iter=1e5)


mod2_sim=coda.samples(model=mod2,variable.names = params2,n.iter = 5e3)

mod2_csim=do.call(rbind,mod2_sim)

summary(mod2_sim)
summary(mod2_csim)
