#model struct 2

state.trans <- function(origin, new.states, params, s.matrix){
  #origin   #single number
  #new.states  #a vector of length n (to index the matrix)
  #params #a vector of length m (to calculate the probabilities)
  #s.matrix  #state.matrix #a matrix cut from the data frame
  
  #dimension check
  if(ncol(s.matrix) <  max(c(origin, new.states))) stop("no such states in the input matrix") #stop if the dim requested is higher than input matrix
  
  ORIGIN <- s.matrix[,origin] #initializing a new vector for calculation
  lo <- length(ORIGIN) #length of origin
  org.s.matrix <- s.matrix    #keeping the original matrix
  
  
  for(i in new.states){
    probs <- rep(1-exp(-params[which(new.states==i)]*1), lo) #calculating probs for transition
    rand <- runif(lo)
    
    s.matrix[,i] <- org.s.matrix[,i]+(ORIGIN*(rand<probs))
  }
  s.matrix
}
