# setting up a bandit-model
install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags, parallel)
#pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr)

set.seed(1983)

### NB! DON'T FORGET TO SET YOUR WORKING DIRECTORY
# setwd("WHERE/YOUR/SCRIPTS/ARE")

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


# payoff structure
# generate a payoff matrix for the bandit task
# Choice of bandit A = 30% chance of 2. Choice of bandit B = 80% chance of 1. Otherwise nothing.
# Yup, I know, I rigged the bandits a bit compared to before... but you know, our guy on the boat
# needs a bit of help if he is to learn anything here...
ntrials <- 100 # increasing this will take a while for the big loop to finish 
Aprob <- .3
Arew <- 2
Bprob <- .8
Brew <- 1

# there's all sorts of ways to generate a payoff matrix. We can use conditions or whatever.
# but let's use binomials because it's really efficient
# why do we mulitply the output of the binomial draw?
payoff <- cbind(rbinom(ntrials,1,Aprob)*Arew, rbinom(ntrials,1,Bprob)*Brew)

# let's look at which option is best in the long run
colSums(payoff)

########################################################################
#--------------RW & softmax model---------------------------------------
########################################################################
# now lets call the function for the RW model that we wrote
# first we need to give the agent a learning rate (a) and an inverse heat (beta) parameter

a <- .1
beta <- .05 # higher number means more consistent choice behavior (aka. less exploration)

source("RW.R") # don't forget to check that you have the right working directory
RW_sims <- RW(payoff,ntrials,a,beta)

## lets make some plots of the expected utilities and 
par(mfrow=c(3,1))
plot(RW_sims$Q[,1], main="Expected utility for bandit 1")
plot(RW_sims$Q[,2], main="Expected utility for bandit 2")
plot(RW_sims$x, main="Actual choice")

# ----------applying the RW & softmax model to the simulated data--------
# now we have some simulated data from a learning agent. 
# Let's see if we can recover the model parameters using inference with jags

x <- RW_sims$x
r <- RW_sims$r

data <- list("x","r","ntrials") 

params<-c("a", "beta")

samples <- jags.parallel(data, inits=NULL, params,
                model.file ="RW.txt",
                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

X <- samples$BUGSoutput$sims.list

a <- X$a
beta <- X$beta

par(mfrow=c(2,1))
plot(density(a))
plot(density(beta))


### NOW: FULL THROTTLE

# 100 iterations take approx. 10 mins (with 100 trials) and 1h 45m (with 500 trials)
niterations <- 100 # this may take a while...
true_a <- array(0,c(niterations))
true_beta <- array(0,c(niterations))

infer_a <- array(0,c(niterations))
infer_beta <- array(0,c(niterations))

# checking the runtime on our parameter recovery
start_time = Sys.time()

for (i in 1:niterations) {
  
  # let's see how robust the model is. Does it recover all sorts of values?
  a <- runif(1,0,1)
  beta <- rgamma(1,1,1) #set up for use in categorical dist, so we get resp of 1 or 2
  
  #run function and extract responses
  RW_sims <- RW(payoff,ntrials,a,beta)
  x <- RW_sims$x
  r <- RW_sims$r
  
  # set up jags and run jags model
  data <- list("x","r","ntrials") 
  params<-c("a","beta")
  
  # introducing "parallel" computing for JAGS (only helps for the number of chains that you run)
  samples <- jags.parallel(data, 
                           inits=NULL, 
                           params,
                           model.file ="RW.txt",
                           n.chains=3, 
                           n.iter=5000, 
                           n.burnin=1000, 
                           n.thin=1,
                           n.cluster=3)

  true_a[i] <- a
  true_beta[i] <- beta
  
  # find maximum a posteriori
  X <- samples$BUGSoutput$sims.list
  infer_a[i] <- MPD(X$a)
  infer_beta[i] <- MPD(X$beta)
  
  print("Done running iteration number:")
  print(i)

}

end_time = Sys.time()
print("Runtime for RW model: ")
end_time - start_time

par(mfrow=c(3,1))
plot(true_a,infer_a, main="learning rate (a)")
plot(true_beta,infer_beta, main="inverse heat (beta)")
plot(true_beta,infer_beta, main="inverse heat (beta) | log-log", log='xy')
                





