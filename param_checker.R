#param_checker

param_checker <- function(x){
  if(is.list(x)){
    y <- NA
    for(i in 1:length(param)){
      y[i] <- length(x[[i]][2]) == length(x[[i]][3])
    }
    if(all(y)){"Input is correct!!!"}
    #"Input is a list"
  }
  else {"Input is not a list"}
}