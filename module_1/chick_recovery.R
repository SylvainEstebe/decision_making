# Building models
install.packages("R2jags")
library(R2jags)

set.seed(1983)

MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#-------- Model 1 - fixed skill level (theta) ------
#simulation - running an experiment as if we knew the process and the parameter
# "G" is for "Guess" 

ntrials <- 100
Gfixed <- array(NA, c(ntrials))
theta <- 0.7

for (t in 1:ntrials) {
  Gfixed[t] <- rbinom(1,1,theta)
}

plot(Gfixed)

# -------- Fit data to model

data <- list("Gfixed", "ntrials")
params <- c("theta")

fixed_samples <- jags(data, inits=NULL, params, model.file="chick_jags.txt", n.chains = 3, 
                      n.iter = 5000, n.burnin = 1000, n.thin=1)

plot(density(fixed_samples$BUGSoutput$sims.list$theta))

#-------- Model 2 - learning model ------
#simulation - running an experiment as if we knew the process and the parameter

ntrials <- 100
Glearn <- array(NA, c(ntrials))

theta_learn <- array(NA, c(ntrials))
alpha <- 0.05
theta1 <- 0.5

theta_learn[1] <- theta1
Glearn[1] <- rbinom(1,1,theta1)

for (t in 2:ntrials) {
  theta_learn[t] <- theta_learn[t-1]^(1/(1+alpha))
  Glearn[t] <- rbinom(1,1,theta_learn[t])
}

#plot(theta_learn)
#plot(Glearn, col=2) # plotting in red

# mimic "hold on" - i.e. overlay two plots
#plot(Gfixed)
#points(Glearn, col=2) # overlaying in red


# ------- fit learning simulations to learning model

# -------- Fit data to model

data <- list("Glearn", "ntrials")
params <- c("theta1", "alpha", "theta_learn")

learn_samples <- jags(data, inits=NULL, params, model.file="chick_learn_jags.txt", n.chains = 3, 
                      n.iter = 5000, n.burnin = 1000, n.thin=1)

plot(density(learn_samples$BUGSoutput$sims.list$theta1))
plot(density(learn_samples$BUGSoutput$sims.list$alpha))

X <- learn_samples$BUGSoutput$sims.list
alpha_recov <- MPD(X$alpha)
theta1_recov <- MPD(X$theta1)
