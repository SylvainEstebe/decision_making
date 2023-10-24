# setting up a bandit-model
install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags)
#pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr)

set.seed(1983)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


# payoff structure
# generate a payoff matrix for the bandit task
# Choice of bandit A = 30% chance of 2. Choice of bandit B = 70% chance of 1. Otherwise nothing.
ntrials <- 100
Aprob <- .3
Arew <- 2
Bprob <- .7
Brew <- 1

# there's all sorts of ways to generate a payoff matrix. We can use conditions or whatever.
# but let's use binomials because it's really efficient
# why do we mulitply the output of the binomial draw?
payoff <- cbind(rbinom(ntrials,1,Aprob)*Arew, rbinom(ntrials,1,Bprob)*Brew)

# let's look at which option is best in the long run
colSums(payoff)

theta <- .7 # bias for one bandit over the other
b <- c(theta, 1-theta) # stated in terms of our categorical choice probabilities

# Don't forget to set your working directory
#setwd("Where/Are/My/Files")

source("random.R") # NB! working directory
random_sims <- random(payoff, ntrials, b)

a <- 0.05
beta <- 0.05
source("RW.R")
random_sims <- random(payoff, ntrials, a, beta)

##### fit data to model #####
x <- random_sims$x

data <- list("x", "ntrials")

params <- c("b")

samples <- jags(data, inits=NULL, params,
                model.file = "random.txt",
                n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin=1)

X <- samples$BUGSoutput$sims.list
leftBias <- X$b[,1]
rightBias <- X$b[,2]

par(mfrow=c(2,1))
plot(density(leftBias))
plot(density(rightBias))

### NOW: FULL THROTTLE
niterations <- 100
true_theta <- array(0, c(niterations))
infer_b1 <- array(0, c(niterations))
infer_b2 <- array(0, c(niterations))
infer_theta <- array(0, c(niterations))

for (i in 1:niterations) {
  
  theta <- runif(1,0,1)
  b <- c(theta, 1-theta)
  
  random_sims <- random(payoff, ntrials, b)
  
  x <- random_sims$x
  
  data <- list("x", "ntrials")
  
  params <- c("b", "theta")
  
  samples <- jags(data, inits=NULL, params,
                  model.file = "random.txt",
                  n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin=1)
  
  true_theta[i] <- theta
  
  X <- samples$BUGSoutput$sims.list
  
  infer_b1[i] <- MPD(X$b[,1])
  infer_b2[i] <- MPD(X$b[,2])
  infer_theta[i] <- MPD(X$theta)
  
}

par(mfrow=c(2,2))
plot(true_theta, infer_b1)
plot(1-true_theta, infer_b2)
plot(infer_b1, infer_b2)
plot(true_theta, infer_theta)

                





