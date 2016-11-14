#function for running state_trans function

source("D:\\Dropbox\\IBM project_Sai\\generic functions\\syn_pop.R")
source("D:\\Dropbox\\IBM project_Sai\\generic functions\\state_trans.R")
pop <- syn_pop(c(19,1,0,0)) #synthesizing population
beta <- 2 #effective contact rate
#lambda = beta*sum(pop[,2],pop[,3])/sum(pop) #force of infection

#prameters for transitions
trans_para <- list(
  list(1,2,NA), #transition from state 1 to 2 using FOI lambda
  list(2,3,100), #transition from state 2 to 3, the 3rd term 100 ensures the near 100% transition to the next stage
  list(3,4,100)
)
no.of.timesteps <- 10
transient <- c("trans_para[[1]][[3]] <- beta*sum(pop[,2],pop[,3])/sum(pop)")

eval(parse(text=transient))

run_state_trans <- function(no.of.timesteps, trans_para, pop, transient){
  Matrix.List <- list() #master matrix list initiazilation, to store the transition values each timestep
  sim.table <- matrix(NA, no.of.timesteps, ncol(pop))  #table to record the summaries each time step
  for(i in 1:no.of.timesteps){
    for(j in 1:length(trans_para)){
      Matrix.List[[j]] <- state_trans(trans_para[[j]][[1]], trans_para[[j]][[2]], trans_para[[j]][[3]], pop)
    }
    pop <- Reduce('+', Matrix.List) + pop #Population after transition
    
    #this is the problem
    #trans_para[[1]][[3]] <- beta*sum(pop[,2],pop[,3])/sum(pop) #recalculating FOI
    #but it can potentially solve it
    #eval(parse(text="b <- beta*sum(pop[,2],pop[,3])/sum(pop)"))
    # > tmp <- c("e <- beta*sum(pop[,2],pop[,3])/sum(pop)","d<- beta*sum(pop[,2],pop[,3])/sum(pop)")
    # > eval(parse(text=tmp))
    eval(parse(text=transient))
    sim.table[i,] <- colSums(pop) #getting summaries of the population
  }
  sim.table
}

run_state_trans(no.of.timesteps,trans_para,pop,transient)

