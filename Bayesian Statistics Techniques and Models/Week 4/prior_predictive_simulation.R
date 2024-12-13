dat=read.table(file="cookies.dat",header=TRUE)
head(dat)
table(dat$location)
boxplot(chips~location,data=dat)

set.seed(112)
n_sim=500

alpha_pri=rexp(n_sim,rate=1/2)
beta_pri=rexp(n_sim,rate=5.0)

mu_pri=alpha_pri/beta_pri
sig_pri=sqrt(alpha_pri/beta_pri^2)

summary(mu_pri)
summary(sig_pri)

lam_pri=rgamma(n_sim,shape = alpha_pri,rate = beta_pri)
summary(lam_pri)


y_pri=rpois(n_sim,lam_pri)
summary(y_pri)

str(lam_pri)
str(dat)
lam_pri=lam_pri[1:5]
lam_pri
y_pri=rpois(150,lambda = rep(lam_pri,each=30))
y_pri
