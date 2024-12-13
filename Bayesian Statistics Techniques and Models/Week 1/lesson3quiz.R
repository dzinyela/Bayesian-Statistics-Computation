#Question 5

#set seed to get consistent results
set.seed(32)

m=10000 #montecarlo sample size
a=5 #shape parameter 
b=3 #rate parameter


#use rgama to simulate, create random gamma distribution 
theta=rbeta(n=m,shape1=a,shape2=b)

mean(theta/(1-theta))

#question 6
odd_success <- theta/(1-theta)

mean(odd_success>1)


#question 7
theta1=rnorm(m,mean=0,sd=1)
quantile(theta1,probs=0.3)
