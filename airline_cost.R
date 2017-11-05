
#################################################
# step 1: import data
dataname = "./airline_costs.dat"
data <- read.delim(dataname, header = FALSE, sep="", skip=0, as.is=TRUE)
#data <-read.table(dataname, header=FALSE)
colnames(data) <- c("airline","leng", "speed", "time", "pop", "cost", "rev", "tmlf", "cap", "ta", "invest", "aa")


d0 <- data[,2:12]
d0$log_leng <- log(d0$leng)
d0$log_speed <- log(d0$speed)
d0$log_time <- log(d0$time)
d0$log_pop <- log(d0$pop)
d0$log_cost <- log(d0$cost)
d0$log_rev <- log(d0$rev)
d0$log_tmlf <- log(d0$tmlf)
d0$log_cap <- log(d0$cap)
d0$log_aa <- log(d0$aa)

dat = na.omit(d0)
pairs(dat)
plot(log_cost ~ log_leng, data = dat)
plot(log_cost ~ log_speed, data = dat)
plot(log_cost ~ log_time, data = dat)
plot(log_cost ~ log_pop, data = dat)
plot(log_cost ~ log_tmlf, data = dat)
plot(log_cost ~ log_cap, data = dat)
plot(log_cost ~ log_aa, data = dat)

################################################################
# step 2: build MCMC model

library("rjags")

mod1_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*log_leng[i] + b[3]*log_speed[i] + b[4]*log_time[i] + b[5]*log_pop[i] + b[6]*log_tmlf[i] + b[7]*log_cap[i] + b[8]*log_aa[i]
}

for (i in 1:8) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(
  y = dat$log_cost,
  n = nrow(dat),
  log_leng = dat$log_leng,
  log_speed = dat$log_speed,
  log_time = dat$log_time,
  log_pop = dat$log_pop,
  log_tmlf = dat$log_tmlf,
  log_cap = dat$log_cap,
  log_aa = dat$log_aa
 
)

params1 = c("b", "sig")

#inits1 = function() {
#  inits = list("b" = rnorm(8, 0.0, 100.0), "prec" = rgamma(1, 1.0, 1.0))
#}

mod1 = jags.model(
  textConnection(mod1_string),
  data = data1_jags,
  n.chains = 3
)


################################################################
# step 3: sampling

update(mod1, 100000) # burn-in

# coda.samples: Generate posterior samples in mcmc.list format
mod1_sim = coda.samples(model = mod1,
                        variable.names = params1,
                        n.iter = 5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains



graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
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







