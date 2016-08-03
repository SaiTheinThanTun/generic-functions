#this is a function for synthesizing the states of population in a matrix with 0s and 1s 
#when an input of vector (each value representing the number of individuals in that state) is given

#length of the input vector gives out the number of states (which is also total columns)
#sum of the vector gives the total population size (which is also total rows)
#proportions of each state is calculated by dividing the vector with total population size

syn_pop <- function(states){ #states is the vector variable, each element represent the number of individuals belonging to that state (indexed)
  #this is assuming that an individual can have only one state at a single timestep
  
  no.states <- length(states) #number of states, decide the columns
  total.pop <- sum(states) #total population size
  
  probs <- states/total.pop #probabilities of being in the specific states
  
  result <- sample(0:1, total.pop, replace=T, prob=c(1-probs[1], probs[1])) #to store the resulting matrix, columns will be binded here in the following for loop
  
  
  for(i in 2:no.states){
    tmp <- sample(0:1, total.pop, replace=T, prob=c(1-probs[i], probs[i])) #tmp to store the result before cbinding
    
    result <- cbind(result, tmp, deparse.level = 0)
  }
  result
}