library("coda")
autocorr.plot(as.mcmc(post$mu))

set.seed(61)
post0 = mh(n=n, ybar=ybar, n_iter=10e3, mu_init=0.0, cand_sd=0.9)
coda::traceplot(as.mcmc(post0$mu[-c(1:500)]))

set.seed(61)
post1 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post1$mu[-c(1:500)]))

set.seed(61)
post2 = mh(n=n, ybar=ybar, n_iter=100e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post2$mu))


coda::autocorr.plot(as.mcmc(post0$mu))


coda::autocorr.plot(as.mcmc(post1$mu))


coda::autocorr.diag(as.mcmc(post1$mu))

str(post2) # contains 100,000 iterations

coda::effectiveSize(as.mcmc(post2$mu)) # effective sample size of ~350

## thin out the samples until autocorrelation is essentially 0. This will leave you with approximately independent samples. The number of samples remaining is similar to the effective sample size.
coda::autocorr.plot(as.mcmc(post2$mu), lag.max=500)

thin_interval=400
thin_indx=seq(from=400,100e3, by=400)

