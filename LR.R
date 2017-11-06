library("car")
data("Leinhardt")
?Leinhardt
head(Leinhardt)

str(Leinhardt)

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data=Leinhardt)

lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)

dat = na.omit(Leinhardt)

library("rjags")


mod1_string = " model {
    for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*log_income[i] 
}

for (i in 1:2) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains


plot(mod1_sim)

gelman.diag(mod1_sim)


autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)

summary(mod1_sim)




library("rjags")

mod2_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
}

for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt( 1.0 / prec )
} "


set.seed(73)
data2_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data2_jags$is_oil

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains

plot(mod2_sim)


