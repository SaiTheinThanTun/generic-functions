#generic function to average out a list/stack of identical tables

#required inputs
#sims: a list of identical simulation/tables
#function to use: default is a mean function
#CI to use if function is defined as ci: default is .05

avg_stk_tbl <- function(sims, f ='avg', ci=.05){
  tmp_avg <- rep(NA,length(sims))
  avg_sims <- matrix(NA,nrow(sims[[1]]),ncol(sims[[1]])) #initializing a blank dataset of summary table
  for(i in 1:ncol(sims[[1]])){ #outer loop for the columns
    for(j in 1:nrow(sims[[1]])){ #inner loop for the rows
      for(k in 1:length(sims)){#innermost loop for no. of simulations(3rd dimension)
        tmp_avg[k] <- sims[[k]][j,i]
      }
      if(f=='avg'){
        avg_sims[j,i] <- mean(tmp_avg)  
      }
      else if(f=='ci'){
        avg_sims[j,i] <- quantile(tmp_avg, probs=ci, na.rm=TRUE)
      }
    }
  }
  avg_sims
}
#no_sims: number of simulations/ thickness of the stacked table is no longer required