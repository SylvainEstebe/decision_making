RW <- function(payoff, ntrials, a, beta) {  

  x <- array(0, c(ntrials))
  r <- array(0, c(ntrials))
  Q <- array(0, c(ntrials,2))
  Qupdate <- array(0, c(ntrials,2))
  exp_p <- array(0, c(ntrials,2))
  p <- array(0, c(ntrials,2))
  
  #set value for trial 1
  Q[1,1] <- round(mean(colSums(payoff))/ntrials)
  Q[1,2] <- round(mean(colSums(payoff))/ntrials)
  
  for (t in 2:ntrials) {
    
    for (k in 1:2) {
      Qupdate[t,k] <- Q[t-1, k] + (a*(r[t-1]-Q[t-1,k]))
      Q[t,k] <- ifelse(k==x[t-1], Qupdate[t,k], Q[t-1, k])
      
      exp_p[t,k] <- exp(beta*Q[t,k])
    }
    
    for (k in 1:2) {
      
      p[t,k] <- exp_p[t,k]/sum(exp_p[t,])
      
    }
    
    x[t] <- rcat(1, p[t,])
    
    r[t] <- payoff[t,x[t]]
  
  }

  result <- list(x=x,
                 r=r,
                 Q=Q)
  
  return(result)
}