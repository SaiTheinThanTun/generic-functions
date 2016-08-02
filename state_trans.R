#model struct 2

state.trans <- function(origin, new.states, params, s.matrix){
  #origin   #single number
  #new.states  #a vector of length n (to index the matrix)
  #params #a vector of length m (to calculate the probabilities)
  #s.matrix  #state.matrix #a matrix cut from the data frame
  
  #dimension check
  if(ncol(s.matrix) <  max(c(origin, new.states))) stop("no such states in the input matrix") #stop if the dim requested is higher than input matrix
  
  origin_v <- s.matrix[,origin] #initializing a new vector for calculation
  lo <- length(origin_v) #length of origin
  org.s.matrix <- s.matrix    #keeping the original matrix ??? for what?
  
  #cummulative probability
  probs <- 1-exp(-params*1) # calc probs from rates
  compliments <- 1-probs
  sum_compliments <- sum(compliments)
  maxprobs <- sum(probs,compliments)
  cum_probs <- cumsum(c(sum_compliments,probs)/maxprobs)
  
  last_prob <- cum_probs[1]
  
  for(i in new.states){
    probs_for <- cum_probs[which(new.states==i)+1] #calculating probs_for for transition
    rand <- runif(lo)
    
    s.matrix[,i] <- s.matrix[,i]+(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob)) #origin is used here since ??
    s.matrix[,origin] <- s.matrix[,origin]-(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob))
    
    last_prob <- probs_for
  }
  s.matrix
}
