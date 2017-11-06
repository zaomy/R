df <- read.table("agedeath.dat", header = FALSE)
colnames(df) <- c('country', 'age', 'index')
boxplot(age ~ country, data=df)


d1 <- df[df$V1=='aris',]
hist(d1$V2)

lmod = lm(age ~ country, data=df)
summary(lmod)
anova(lmod)
plot(lmod)
model.matrix(lmod)

#####################

library("rjags")
mod_string = " model {
    for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec)
}

for (j in 1:3) {
mu[j] ~ dnorm(60.0, 1.0/100.0)
}

prec ~ dgamma(18.0, 340.0)
sig = sqrt( 1.0 / prec )
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=df$age, 
                 grp=as.numeric(df$country))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,60.0,100.0), "prec"=rgamma(1,18.0,340.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains


plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

(pm_params = colMeans(mod_csim))


yhat = pm_params[1:3][df$country]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

summary(mod_sim)

HPDinterval(mod_csim)

mean(mod_csim[,3] < mod_csim[,1])
