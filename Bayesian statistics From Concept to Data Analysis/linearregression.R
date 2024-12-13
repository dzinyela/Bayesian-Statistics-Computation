##read data from online
oring=read.table("http://www.randomservices.org/random/data/Challenger2.txt",header=T)

attach(oring)
oring

##visualise
plot(T,I)

#perform linear regresion
oring.lm=lm(I~T)

summary(oring.lm)

#plot fitted line
lines(T,fitted(oring.lm))


#95% posterior interval for the slope
-0.24337-0.06349*qt(0.975,21)

-0.24337+0.06349*qt(0.975,21)


#predict when temperature is 31
#y=mx+c
yhat=-0.24337*31+18.36508
yhat


#use coeff command to extract a vector of coefficienrs
coef(oring.lm)
coef(oring.lm)[1]+coef(oring.lm)[2]*31


#using the predict command
yhat2=predict(oring.lm,data.frame(T=31),interval='predict')
yhat2

#posterior prediction interval like a frequantist (using long equation)
10.82052-2.102*qt(0.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))

#posterior prediction that damage is greater than 0
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)


#Read new data set
heights=read.table("http://www.randomservices.org/random/data/Galton.txt",header=T)
heights



attach(heights)
names(heights)

pairs(heights)
summary(lm(Height~Father+Mother+Gender+Kids))
summary(lm(Height~Father+Mother+Gender))
heights.lm=lm(Height~Father+Mother+Gender)


# 95% posterior interval for the the difference in height by gender
5.226 - 0.144*qt(.975,894)
5.226 + 0.144*qt(.975,894)


# posterior prediction interval (same as frequentist)
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="M"),interval="predict")
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="F"),interval="predict")



##Exercise
golf=read.table('http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat',header=T)
golf


datF <- subset(golf, X1==1, select=1:2)

datM <- subset(golf, X1==2, select=1:2)

attach(datF)

#creat a plot
plot(datF$X243.2,datF$X67.0)

plot(datM$X243.2,datM$X67.0)

#Fit and PREDICTION
datF.lm=lm(datF$X67.0~datF$X243.2)

datF.lm

predict(datF.lm,data.frame(X243.2=260),interval="predict")

130.9995-0.2569*260


datamining=load("C:/Users/dziny/Downloads/class_data.RData")
datamining
