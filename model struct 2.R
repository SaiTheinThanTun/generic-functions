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
  org.s.matrix <- s.matrix    #keeping the original matrix
  
  #cummulative probability
  probs <- 1-exp(-params*1) # calc probs from rates
  compliments <- 1-probs
  sum_compliments <- sum(compliments)
  maxprobs <- sum(probs,compliments)
  cum_probs <- c(sum_compliments,probs)/maxprobs
  
  for(i in new.states){
    probs_for <- rep(cum_probs[which(new.states==i)+1], lo) #calculating probs_for for transition
    rand <- runif(lo)
    
    s.matrix[,i] <- org.s.matrix[,i]+(origin_v*(rand<probs_for))
    s.matrix[,origin] <- org.s.matrix[,origin]-(origin_v*(rand<probs_for))
  }
  s.matrix
}
