# Building models
install.packages("R2jags")
library(R2jags)

set.seed(1983)

#-------- Model 1 - poor starter, fast learner ------
#simulation - running an experiment as if we knew the process and the parameter
# "G" is for "Guess" 

ntrials <- 100
Glearn <- array(NA, c(ntrials))

theta_learn <- array(NA, c(ntrials))
alpha <- 0.1
theta1 <- 0.1

theta_learn[1] <- theta1
Glearn[1] <- rbinom(1,1,theta1)

for (t in 2:ntrials) {
  theta_learn[t] <- theta_learn[t-1]^(1/(1+alpha))
  Glearn[t] <- rbinom(1,1,theta_learn[t])
}

plot(theta_learn)
plot(Glearn) # plotting in red


#-------- Model 2 - "standard" starter/learner ------
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

plot(theta_learn)
plot(Glearn, col=2) # plotting in red

