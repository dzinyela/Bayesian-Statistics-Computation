
#we don't assume we know sigma squared or mu
#we use normal for mu and inverse gamma for sigma squared

#update mu
update_mu=function(n,ybar,sig2,mu_0,sig2_0) {
  sig2_1=1.0/(n/sig2 + 1.0/sig2_0)
  mu_1=sig2_1 *(n*ybar/sig2 + mu_0/sig2_0)
  rnorm(n=1.0,mean=mu_1,sd=sqrt(sig2_1))
}

#update sig2
update_sig2=function(n,y,mu,nu_0,beta_0) {
  nu_1=nu_0+n/2.0
  sumsq=sum((y-mu)^2)
  beta_1=beta_0+sumsq/2.0
  out_gamma=rgamma(n=1.0,shape=nu_1,rate=beta_1)
  1/out_gamma
}


##create gibbs sampler
gibbs=function(y,n_iter,init,prior) {
  ybar=mean(y)
  n=length(y)
  
  mu_out=numeric(n_iter)
  sig2_out=numeric(n_iter)
  
  mu_now=init$mu
  
  ##Gibbs sampler
  for (i in 1:n_iter) {
    sig2_now=update_sig2(n=n,y=y,mu=mu_now,nu_0=prior$nu_0,
beta_0=prior$beta_0)
    mu_now=update_mu(n=n,ybar=ybar,sig2=sig2_now,mu_0=prior$mu_0,
                     sig2_0=prior$sig2_0)
    
    sig2_out[i]=sig2_now
    mu_out[i]=mu_now
  }
  cbind(mu=mu_out,sig2=sig2_out)
  
  
  
}


#set up the problem
#y=c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.3,1.9)
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
ybar=mean(y)
n=length(y)


##priors
prior=list()

prior$mu_0=1.0
prior$sig2_0=1.0
prior$n_0=2.0   ###prior effective sample size
prior$s2_0=1.0    ### initial guess for sigma squared

prior$nu_0=prior$n_0/2.0
prior$beta_0=prior$n_0*prior$s2_0/2.0


hist(y, freq=FALSE, xlim=c(-1.0, 3.0)) # histogram of the data
curve(dnorm(x=x, mean=prior$mu_0, sd=sqrt(prior$sig2_0)), lty=2, add=TRUE) # prior for mu
points(y, rep(0,n), pch=1) # individual data points
points(ybar, 0, pch=19) # sample mean



##run gibbs sampler
set.seed(53)

init=list()
init$mu=0.0


##call gibbs function
post=gibbs(y=y,n_iter=5e3,init=init,prior=prior)


library("coda")
plot(as.mcmc(post))


summary(as.mcmc(post))
