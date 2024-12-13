#set seed to get consistent results
set.seed(32)

m=10000 #montecarlo sample size
a=2.0 #shape parameter 
b=1/3 #rate parameter


#use rgama to simulate, create random gamma distribution 
theta=rgamma(n=m,shape=a,rate=b)

#finding standard error of our montecarlo values
se =sd(theta)/sqrt(m)
se

#creating the confident intervals
mean(theta)-2*se
mean(theta)+2*se

#using indicator to find values below 5
ind=theta<5

#find probability theta is less than 5
mean(ind) #probability is less than 5 for simulated values
pgamma(5, shape =a,rate=b)

se=sd(ind)/sqrt(m) #standard error for the indicator variable
se
2*se
mean(ind)+2*se
mean(ind)-2*se


#simulate for heirarichal model
m=1e5 #m=100000
y=numeric(m)
phi=numeric(m) #create a zeeros of m size

for(i in 1:m) {
 phi[i]=rbeta(1,shape1=2,shape2=2)
 y[i]=rbinom(1,size=10,prob=phi[i])
}

#writing the same  code but vectorized
phi =rbeta(m,shape1=2,shape2=2)
y=rbinom(m,size=10,prob=phi)

#table of y
plot(table(y)/m) #montecarlo approximation of distribution of y

