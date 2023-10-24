### RW blocking simulation (do this at home)

# blocking of simple learning relationship

# rates - must be from 0 - 1  
alphaA <- 1 # play with this to change the saliency of the conditioned stimulus (CS, e.g. sound)
alphaX <- 1
beta <- .1 # play with this to change the learning rate, i.e. the saliency of the unconditioned stimulus (US, e.g. blink reflex from airpuff)

ntrials <-  40
nlearn <- floor(ntrials/2) # in case of an uneven number of trials
lambda <- 1

VA <- array(NA, ntrials)
VA[1] <- 0

VX <- array(0, ntrials) # Should ideally have been NAs, but cuz Andreas' is lazy, we work with zeros
VX[1] <- 0

VAX <- array(NA, ntrials)
VAX[1] <- 0

delta_VA <- array(NA, ntrials)
delta_VA[1] <- 0

delta_VX <- array(NA, ntrials)
delta_VX[1] <- 0

for (t in 2:nlearn) {
  
  delta_VA[t] <- alphaA * beta * (lambda - VAX[t-1])
  VA[t] <- VA[t-1] + delta_VA[t]
  # delta_VX[t] <- alphaX * beta * (lambda - VAX[t-1])
  # VX[t] <- VX[t-1] + delta_VX[t]
  VAX[t] <- VA[t] + VX[t]
  
}

for (t in (nlearn+1):ntrials) {
  
  delta_VA[t] <- alphaA * beta * (lambda - VAX[t-1])
  VA[t] <- VA[t-1] + delta_VA[t]
  delta_VX[t] <- alphaX * beta * (lambda - VAX[t-1])
  VX[t] <- VX[t-1] + delta_VX[t]
  VAX[t] <- VA[t] + VX[t]
  
}


par(mfrow=c(2,2))
plot(VAX)
plot(VA, ylim=c(0,1))
plot(VX, ylim=c(0,1))
