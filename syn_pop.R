#this is a function for synthesizing the states of population in a matrix with 0s and 1s 
#when an input of vector (each value representing the number of individuals in that state) is given

#length of the input vector gives out the number of states (which is also total columns)
#sum of the vector gives the total population size (which is also total rows)
#proportions of each state is calculated by dividing the vector with total population size

syn_pop <- function(states, shuffle=FALSE){ #states is the vector variable, each element represent the number of individuals belonging to that state (indexed)
  #this is assuming that an individual can have only one state at a single timestep
  
  total.pop <- sum(states) #total population size
  
  result <- c(rep(1,states[1]), rep(0,total.pop - states[1])) #to store the resulting matrix, columns will be binded here in the following for loop
  
  
  for(i in 2:length(states)){
    tmp <- c(rep(0,sum(states[1:(i-1)])), rep(1, states[i]) , rep(0,total.pop - sum(states[1:i]) )) #tmp to store the result before cbinding
    
    result <- cbind(result, tmp, deparse.level = 0)
  }
  
  if(shuffle==T){ result[sample(nrow(result)),]}
  else result
}