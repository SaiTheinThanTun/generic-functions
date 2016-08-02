#model structure
# n.cases <- 12
# n.states <- 3
# stateM <- matrix(rep(diag(n.states),ceiling(n.cases/n.states)),n.cases,n.states, byrow=T)
# 
# stateM

# colSums(stateM)
# 
# 
# tmp <- list()
# tmp[[1]] <- 123
# tmp[[2]] <- "x"
# tmp[[3]] <- function(b) {print(b)}
# tmp
# 
# StateChange <- list()
# StateChange[[1]] <- function(states, lambda){
#   
# }
origin <- 1
des <- c(2,3)
params <- c(.5,1)

A <- c(rep(1,10),rep(0,10))
B <- c(rep(0,10),rep(0:1,5))
C <- rep(0,20)
M <- cbind(A,B,C)

state.trans(origin, des, params, M)
state.trans(origin, 2, .5, M)
state.trans(origin, 3, .5, M)


params <- c(.2,.5,.7)
#testing for cummulative probs
probs <- 1-exp(-params*1) # calc probs from rates
compliments <- 1-probs
sum_compliments <- sum(compliments)
maxprobs <- sum(probs,compliments)
cum_probs <- c(sum_compliments,probs)/maxprobs
cum_probs
