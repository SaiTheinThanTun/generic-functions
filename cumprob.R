#####explaination####
#this functions takes in a vector of probabilities of states transitions
#and calculate the probability of staying in the original state
#and output the cumulative probabilities for all possibilities

#####function####
cumprob <- function(probs, actual=FALSE){
  #probs is the probability of transitions to states
  #probability of stay will be calculated automatically
  #actual, default FALSE, will calculate a cumulative probability within 0-1
  #actual, if TRUE, will calculate actual cumulative probabilities which may surpass 1!
  #probs <- x
  
  compliments <- 1-probs
  
  cum_probs2 <- NA
  #stay <- compliments[1]-probs[2] 
  stay <- abs(compliments[1] - sum(probs[-1]))
  result <- cumsum(append(stay, probs))
  
  if(actual){
    result
  } else {
    result/result[length(result)]
    }
}

#example
cumprob(c(.2,.2,.9))
cumprob(c(.2,.2,.2))


####draft#####
# probs <- c(.3,.2)
# 
# compliments <- 1-probs
# 
# sum_compliments <- sum(compliments)
# maxprobs <- sum(probs,compliments)
# cum_probs <- cumsum(c(sum_compliments,probs)/maxprobs)
# 
# probs <- c(.3,.2)
# 
# probs <- c(.2,.2,.2)
# 
# compliments <- 1-probs
# 
# cum_probs2 <- NA
# #stay <- compliments[1]-probs[2] 
# stay <- abs(compliments[1] - sum(probs[-1]))
# cumsum(append(stay, probs))