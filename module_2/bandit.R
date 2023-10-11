# setting up a bandit-model
install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags)
#pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr)

set.seed(1983)

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