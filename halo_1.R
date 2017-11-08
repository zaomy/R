#####################################
# step 1, import data
df0 <- read.table("./halo1.dat", header = FALSE)
colnames(df0) <- c('essay', 'appearance', 'score')

df <- df0
df$essay[df$essay==1] <- "Good"
df$essay[df$essay==2] <- "Poor"
df$appearance[df$appearance==1] <- "Pretty"
df$appearance[df$appearance==2] <- "Unaware"
df$appearance[df$appearance==3] <- "Ugly"


boxplot(score ~ essay + appearance, data=df, ylab ="Score", xlab ="Condition (Essay Quality.Appearance)")
boxplot(score ~ essay , data=df, ylab ="Score", xlab ="Condition (Essay Quality)")
boxplot(score ~ appearance, data=df, ylab ="Score", xlab ="Condition (Appearance)")

hist(df$score[df$essay=="Good" & df$appearance=="Pretty"], breaks=20)

######################################
# step 2, two-way cell means model for Bayersian statistics



mod3_string = " model {
  for( i in 1:length(y)) {
    y[i] ~ dnorm(mu[essayGrp[i], appearanceGrp[i]], prec[essayGrp[i], appearanceGrp[i]])
  }

  for (j in 1:max(essayGrp)) {
    for (k in 1:max(appearanceGrp)) {
      mu[j,k] ~ dnorm(15.0, 1.0/5.0^2)
      prec[j,k] ~ dgamma(17.0, 640.0)
      sig[j,k] = sqrt(1.0 / prec[j,k])
    }
  }

} "

# how to determine alpha and beta in prec ~ dgamma(alpha, beta)
# sig^2 ~ invgamma(alpha, beta)
# mu(sig^2) = beta/(alpha - 1) 
# var(sig^2) = beta^2 / [(alpha - 1)^2 * (alpha - 2)]
# looking at the box-plot, I think  mu(sig^2)=25 and var(sig^2)= 25
# solving the two equations, we get
# alpha = 27.0; beta = 130.0 

data3_jags = list(y=df$score, essayGrp=as.numeric(df0$essay), appearanceGrp=as.numeric(df0$appearance))
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
raftery.diag(mod3_sim, q=0.025, r=0.005, s=0.95)

(dic3 = dic.samples(mod3, n.iter=1e3))


######################################
# step 3, Results and conclusions

mean(mod3_csim[,"mu[1,1]"] > mod3_csim[,"mu[1,2]"]) # good-pretty > good-unknown
# mean(mod3_csim[,"sig[1,1]"] > mod3_csim[,"sig[1,2]"]) # good-pretty > good-unknown

mean(mod3_csim[,"mu[2,1]"] > mod3_csim[,"mu[2,2]"]) # poor-pretty > poor-unknown

mean(mod3_csim[,"mu[1,2]"] > mod3_csim[,"mu[1,3]"]) # good-unknown > good-ugly

mean(mod3_csim[,"mu[2,2]"] > mod3_csim[,"mu[2,3]"]) # poor-unknown > poor-ugly

mean(mod3_csim[,"mu[1,1]"] > mod3_csim[,"mu[1,3]"]) # good-pretty > good-ugly

mean(mod3_csim[,"mu[2,1]"] > mod3_csim[,"mu[2,3]"]) # poor-pretty > poor-ugly


mean(mod3_csim[,"mu[1,1]"] > mod3_csim[,"mu[1,3]"]*1.135) # good-pretty > good-ugly

mean(mod3_csim[,"mu[1,2]"] > mod3_csim[,"mu[1,3]"]*1.137) # good-unknown > good-ugly
