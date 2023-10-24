### Simple RW simulation (do this at home)

# simple learning relationship

# rates - must be from 0 - 1  
alphaA <- 1 # play with this to change the saliency of the conditioned stimulus (CS, e.g. sound)
beta <- 1 # play with this to change the learning rate, i.e. the saliency of the unconditioned stimulus (US, e.g. blink reflex from airpuff)

ntrials <-  100
lambda <- 1

VA <- array(NA, ntrials)
VA[1] <- 0

delta_VA <- array(NA, ntrials)
delta_VA[1] <- 0

for (t in 2:ntrials) {
  
  delta_VA[t] <- alphaA * beta * (lambda - VA[t-1])
  VA[t] <- VA[t-1] + delta_VA[t]
  
}

plot(VA)
