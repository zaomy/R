# step 1, import data
df <- read.table("./halo2.dat", header = FALSE)
colnames(df) <- c('essay', 'beauty', 'score')
table(df$beauty, df$score)
boxplot(score ~ essay + beauty, data=df)

#####################################
# step 2, basic ANOVA

lmod = lm(score ~ essay + beauty , data=df)
summary(lmod)
anova(lmod)

######################################
# step 3, one-way ANOVA model for Bayersian statistics

library("rjags")

mod1_string = " model {
for( i in 1:length(y)) {
y[i] ~ dnorm(mu[Grp[i]], prec)
}
for (j in 1:3) {
mu[j] ~ dnorm(15.0, 1.0/5.0^2)
}
prec ~ dgamma(17.0, 640.0)
sig = sqrt(1.0 / prec)
} "

# how to determine alpha and beta in prec ~ dgamma(alpha, beta)
# sig^2 ~ invgamma(alpha, beta)
# mu(sig^2) = beta/(alpha - 1) 
# var(sig^2) = beta^2 / [(alpha - 1)^2 * (alpha - 2)]
# looking at the box-plot, I think  mu(sig^2)=40 and var(sig^2)= 100
# solving the two equations, we get
# alpha = 17.0; beta = 640.0 

set.seed(83)

str(df)

data1_jags = list(y=df$score, Grp=as.numeric(df$essay))

params1 = c("mu", "sig")

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)

update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5e3)

## convergence diagnostics
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)

summary(mod1_sim)
dic1 = dic.samples(mod1, n.iter=1e3)

######################################
# step 4, two-way ANOVA model for Bayersian statistics

X = model.matrix(lm(score ~ essay + beauty, data=df))
head(X)

mod2_string = " model {
for( i in 1:length(y)) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = int + alpha*isEssayPoor[i] + beta[1]*isBeautyControl[i] + beta[2]*isBeautyUnattractive[i] 
}
int ~ dnorm(15.0, 1.0/5.0^2)
alpha ~ dnorm(0.0, 1.0/1.0e6)
for (j in 1:2) {
beta[j] ~ dnorm(0.0, 1.0/1.0e6)
}
prec ~ dgamma(17.0, 640.0)
sig = sqrt(1.0 / prec)
} "

data2_jags = list(y=df$score, isEssayPoor=X[,"essayPoor"], isBeautyControl=X[,"beautyControl"], isBeautyUnattractive=X[,"beautyUnattractive"])
params2 = c("int", "alpha", "beta", "sig")
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)
## convergene diagnostics
plot(mod2_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)


summary(mod2_sim)

(dic2 = dic.samples(mod2, n.iter=1e3))

######################################
# step 4, two-way cell means model for Bayersian statistics

lmod2 = lm(score ~ .^2, data=df)
summary(lmod2)

mod3_string = " model {
for( i in 1:length(y)) {
y[i] ~ dnorm(mu[essayGrp[i], beautyGrp[i]], prec)
}
for (j in 1:max(essayGrp)) {
for (k in 1:max(beautyGrp)) {
mu[j,k] ~ dnorm(15.0, 1.0/5.0^2)
}
}
prec ~ dgamma(17.0, 640.0)
sig = sqrt(1.0 / prec)
} "


str(warpbreaks)
data3_jags = list(y=df$score, essayGrp=as.numeric(df$essay), beautyGrp=as.numeric(df$beauty))
params3 = c("mu", "sig")
mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)
mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))
plot(mod3_sim, ask=TRUE)
## convergence diagnostics
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
effectiveSize(mod3_sim)
raftery.diag(mod3_sim)

(dic3 = dic.samples(mod3, n.iter=1e3))

