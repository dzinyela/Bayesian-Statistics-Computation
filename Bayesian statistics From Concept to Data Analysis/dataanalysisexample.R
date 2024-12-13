#generate data from 0 t0 1 by 0.01
theta=seq(from=0,to=1,by=0.01)

##plot theta
plot(theta,dbeta(theta,1,1), type='l')
plot(theta,dbeta(theta,4,2), type='l')
plot(theta,dbeta(theta,8,4), type='l')

##p beta function
1-pbeta(0.25,8,4)
1-pbeta(0.5,8,4)
1-pbeta(0.8,8,4)

#from  a data, posterior mean given beta(41,11)
posterir_mean<-(41)/(41+11)
posterir_mean




#MLE
33/40


lines(theta,dbeta(theta,41,11))
plot(theta,dbeta(theta,41,11),type='l')
lines(theta,dbeta(theta,8,4),lty=2)




#Week 2
likelihood=function(n,y,theta){return((theta^y)*(1-theta)^(n-y))}
theta=seq(from=0.01,to=0.99,by=0.01)
plot(theta,likelihood(400,72,theta))
abline(v=0.18)

loglikelihood=function(n,y,theta){return(y*log(theta)+(n-y)*(log(1-theta)))}
theta=seq(from=0.01,to=0.99,by=0.01)
plot(theta,loglikelihood(400,72,theta),type="l")
abline(v=0.18)


