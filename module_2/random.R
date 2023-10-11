random <- function(payoff, ntrials, b) {  


  #theta <- .7
  #b <- c(theta, 1-theta)
  
  x <- array(0, c(ntrials))
  r <- array(0, c(ntrials))
  
  for (t in 1:ntrials) {
  
    x[t] <- rcat(1, b)
    
    r[t] <- payoff[t,x[t]]
  
  }

  result <- list(x=x, r=r)
  
  return(result)
}