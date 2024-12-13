#set seed to get consistent results
set.seed(32)

m=10000 #montecarlo sample size
a=2.0 #shape parameter 
b=1/3 #rate parameter


#use rgama to simulate, create random gamma distribution 
theta=rgamma(n=m,shape=a,rate=b)
head(theta)
hist(theta,freq=FALSE) #gives pdf instead of frequency

#use curve to plot the original gamma distribution
curve(dgamma(x,shape=a,rate=b),col="red",add=TRUE)

#get montecarlo approximation to the expected value of thema
sum(theta)/m

#true expected value
a/b

var(theta) #getting variance for the montecarlo

a/b^2 #true variance

#finding probability that theta is less than 5
ind=theta<5
mean(ind) #probability it is less than 5 for the monte carlo

pgamma(q=5.0,shape=a,rate=b) #true probability it is less than 5 from the gamma distribution

#finding quartiles for the distributions
quantile(theta,probs=0.9)
qgamma(p=0.9,shape=a,rate=b)
