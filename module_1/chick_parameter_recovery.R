###### -------- RUN PARAMETER RECOVERY ---#####

install.packages("R2jags")
library(R2jags)

set.seed(1983) # plz try with a seed of your own choice
setwd("~/Documents/git_repository/decision_making/module_1")
# setwd({your_path})

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


#----- Model 1: fixed theta-------

nruns <- 20 # consider only running 20 if you don't wanna wait too long to have a glance at the output
ntrials <- 100

trueTheta <- c(NA,nruns) # array for logging the true theta
inferredTheta <- c(NA,nruns) # array for logging the inferred/estimated theta

# checking the runtime on our parameter recovery
start_time = Sys.time()

for (i in 1:nruns) {
  print(i) # if you wanna keep track of how far you are in nruns...
  
  #simulation - running an experiment as if we knew the process and the parameter
  Gfixed <- array(NA,ntrials)
  
  # here we use a uniform distribution from 0 to 1 for picking random theta values - we could also have used rbeta
  # the uniform distribution is a bit more handy to use for "exploring the parameter space" cuz we 
  # can set the boundaries of the distribution directly in the call
  # so for this purpose of sampling from the possible values of theta, it's not super important 
  # whether we sample from an (uniformed) beta or a uniform distribution, we use whichever
  # is the more practical to use
  theta <- runif(1,0,1) # randomly picking a theta-value (between 0 and 1) for each iteration
  
  for (t in 1:ntrials) {
    
    Gfixed[t] <- rbinom(1,1,theta) 
    
  }
  
  #### fit fixed data to fixed model ############
  data <- list("Gfixed","ntrials") #include theta as data because it is a fixed param
  params <- c("theta")
  
  fixed_samples <- jags(data, inits=NULL, params,
                  model.file ="chick_jags.txt",
                  n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)
  
  trueTheta[i] <- theta
  
  X <- fixed_samples$BUGSoutput$sims.list
  inferredTheta[i] <- MPD(X$theta)
  
}

end_time = Sys.time()
print("Runtime for fixed model: ")
end_time - start_time

# plot true theta against inferred/estimated theta - what does good recovery look like?
plot(trueTheta,inferredTheta, main="theta")


#----- Model 2: learning model-------

nruns <- 100 # consider only running 20 if you don't wanna wait too long to have a glance at the output
ntrials <- 100

trueAlpha <- c(NA,nruns) # array for logging the true alpha
inferredAlpha <- c(NA,nruns) # array for logging the inferred/estimated alpha

trueTheta1 <- c(NA,nruns) # array for logging the true theta1
inferredTheta1 <- c(NA,nruns) # array for logging the inferred/estimated theta1

# checking the runtime on our parameter recovery
start_time = Sys.time()

for (i in 1:nruns) {
  print(i) # if you wanna keep track of how far you are in nruns...
  
  #simulation - running an experiment as if we knew the process and the parameter
  Glearn <- array(NA,ntrials)
  theta_learn <- array(NA,ntrials)
  
  # here we use a uniform distribution from 0 to 1 for picking random theta values - we could also have used rbeta
  # see longer comment above for why we sample from a uniform distribution here
  alpha <- runif(1,0,1) # randomly picking an alpha-value (between 0 and 1) for each iteration
  #print(alpha) # if you wanna print the different values for alpha in the console
  theta1 <- runif(1,0,1) # randomly picking an initial theta-value (between 0 and 1) for each iteration
  #print(theta1) # if you wanna print the different values for alpha in the console
  
  theta_learn[1] <- theta1 
  
  for (t in 2:ntrials) {
    
    theta_learn[t] <- theta_learn[t-1]^(1/(1+alpha)) 
    Glearn[t] <- rbinom(1,1,theta_learn[t]) 
    
  }
  
  data <- list("Glearn","ntrials")
  params <- c("theta_learn","theta1","alpha")
  
  samples <- jags.parallel(data, 
                           inits=NULL, 
                           params,
                           model.file ="chick_learn_jags.txt",
                           n.chains=3, 
                           n.iter=5000, 
                           n.burnin=1000, 
                           n.thin=1,
                           n.cluster=3)
  
  X <- samples$BUGSoutput$sims.list
  
  # recover alpha
  trueAlpha[i] <- alpha
  inferredAlpha[i] <- MPD(X$alpha)
  
  # recover theta1
  trueTheta1[i] <- theta1
  inferredTheta1[i] <- MPD(X$theta1)

}
end_time = Sys.time()
print("Runtime for learning model: ")
end_time - start_time

# setting both x- and y-lims to 0-1 to better appreciate the diagonality
par(mfrow=c(1,2))
plot(trueAlpha,inferredAlpha, main="alpha", xlim=c(0,1), ylim=c(0,1))
plot(trueTheta1,inferredTheta1, main="theta1", xlim=c(0,1), ylim=c(0,1))

# constraining both x- and y-lims to 0-0.2 for alpha to show pseudo-diagonality
par(mfrow=c(1,2))
plot(trueAlpha,inferredAlpha, main="alpha", xlim=c(0,.2), ylim=c(0,.2))
plot(trueTheta1,inferredTheta1, main="theta1", xlim=c(0,1), ylim=c(0,1))

# plotting theta1- and alpha-differences as a function of alpha and theta1, respectively
par(mfrow=c(1,2))
plot(trueAlpha, abs(trueTheta1-inferredTheta1), main="delta-theta1 by alpha")
plot(trueTheta1, abs(trueAlpha-inferredAlpha), main="delta-alpha by theta1")


