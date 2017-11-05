
#################################################
# step 1: import data
dataname = "./nat_fib_cmp.dat"
data <- read.delim(dataname, header = FALSE, sep="", skip=0, as.is=TRUE)
#data <-read.table(dataname, header=FALSE)
colnames(data) <- c("FL","FC", "TS", "FS")


d0 <- data[,1:4]
d0$log_FL <- log(d0$FL)
d0$log_FC <- log(d0$FC)
d0$log_TS <- log(d0$TS)
d0$log_FS <- log(d0$FS)

dat = na.omit(d0)
pairs(dat)
plot(log_TS ~ log_FL, data = dat)
plot(log_TS ~ log_FC, data = dat)

plot(log_FS ~ log_FL, data = dat)
plot(log_FS ~ log_FC, data = dat)

################################################################
# step 2: build MCMC model

library("rjags")

mod1_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*log_FL[i] + b[3]*log_FC[i] 
}

for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(
  y = dat$log_TS,
  n = nrow(dat),
  log_FL = dat$log_FL,
  log_FC = dat$log_FC
  
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







