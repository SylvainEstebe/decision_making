### RW overshadowing simulation (do this at home)

# overshadowing of simple learning relationship

# rates - must be from 0 - 1  
alphaA <- 1 
alphaX <- .5 # NOTE LOWER SALIENCE
beta <- .2 

ntrials <-  40
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

for (t in 2:ntrials) {
  
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
