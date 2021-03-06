
#################################################
# step 1: import data
dataname = "./lsd.dat"
data <-read.table(dataname, header=FALSE)
colnames(data) <- c("tc","score")


data_my <- data

dat = na.omit(data_my)
pairs(dat)
plot(score ~ tc, data = dat)

################################################################
# step 2: build MCMC model

library("rjags")

mod1_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*tc[i]
}

for (i in 1:2) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(
  y = dat$score,
  n = nrow(dat),
  tc = dat$tc
)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b" = rnorm(2, 0.0, 100.0), "prec" = rgamma(1, 1.0, 1.0))
}

mod1 = jags.model(
  textConnection(mod1_string),
  data = data1_jags,
  inits = inits1,
  n.chains = 3
)


################################################################
# step 3: sampling

update(mod1, 1000) # burn-in

# coda.samples: Generate posterior samples in mcmc.list format
mod1_sim = coda.samples(model = mod1,
                        variable.names = params1,
                        n.iter = 5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

plot(mod1_sim)

################################################################
# step 4: check convergence

#coda::gelman.diag(mod1_sim) # show potential scale reduction factors, in case of convergence, it should be close to 1
#coda::gelman.plot(mod1_sim)
gelman.diag(mod1_sim)
gelman.plot(mod1_sim)

autocorr.diag(mod1_sim)

autocorr.plot(mod1_sim)

effectiveSize(mod1_sim) # to get mean value, the effective size should be many thousands


################################################################
# step 5: check residual

summary(mod1_sim)







