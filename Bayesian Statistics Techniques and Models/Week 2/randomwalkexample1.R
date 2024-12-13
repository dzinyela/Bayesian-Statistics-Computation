
lg=function(mu,n,ybar){
  mu2=mu^2
  n*(ybar*mu-mu2/2.0)-log(1+mu2)
}

#do metropolis hasting, mh
#step 1 initialize initial value of unknown parameters
mh=function(n,ybar,n_iter,mu_init,cand_sd){
  mu_out=numeric(n_iter)
  accpt=0
  mu_now=mu_init
  lg_now=lg(mu=mu_now,n=n,ybar=ybar)
  
  
  #step 2(a), repeat the following steps
  
  for(i in 1:n_iter){
    mu_cand=rnorm(1,mean=mu_now,sd=cand_sd)
    
    #compute ratio, 2(b)
    lg_cand=lg(mu=mu_cand,n=n,ybar=ybar)
    
    lg_cand=lg(mu=mu_cand,n=n,ybar=ybar)
    
    #log alpha
    lalpha=lg_cand-lg_now
    
    #exponentiate it to get alpha
    alpha=exp(lalpha)
    
    #step 2(c)
    u=runif(1)
    if(u<alpha){
      mu_now=mu_cand
      accpt=accpt+1
      lg_now=lg_cand
    }
    
    mu_out[i]=mu_now
  }
  
  list(mu=mu_out,accpt=accpt/n_iter)
  
}


##Random walk example, part 2
###set up
y=c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.3,1.9)
ybar=mean(y)
n=length(y)

#plot the ydata as a density
hist(y,freq =FALSE,xlim=c(-1,3))
points(y,rep(0,n)) #plot points on x axis
points(ybar,0,pch=19)#plot mean of y on x axis

#plot t disribution on histogram
curve(dt(x,df=1),lty=2,add=TRUE)




#start posterior sampling
set.seed(43)
post=mh(n=n,ybar=ybar,n_iter=1e3,mu_init=30.0,cand_sd=0.9)

str(post)

install.packages("coda")
library("coda")

traceplot(as.mcmc(post$mu))


#plot posterior against the prior
post$mu_keep=post$mu[-c(1:100)]
plot(density(post$mu_keep),xlim=c(-1,3)) #posterior distribution
curve(dt(x,df=1),lty=2,add=TRUE) #prior distribution
points(ybar,0,pch=19)#plot mean of y on x axis



