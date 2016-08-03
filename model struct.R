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
state.trans(2, c(1,3), c(.5,.8), M)

D <- c(rep(0,10),rep(1:0,5))
MD <- cbind(M,D)
state.trans(1,c(3,4),c(.6,.3),MD)

aaa <- {origin=1,des=c(3,4),params=c(.6,.3),matrix=MD}
bbb <- c(origin=1,new.states=c(3,4),params=c(.6,.3))
state.trans(bbb,s.matrix=MD)


params <- c(.2,.5,.7)
#testing for cummulative probs
probs <- 1-exp(-params*1) # calc probs from rates
compliments <- 1-probs
sum_compliments <- sum(compliments)
maxprobs <- sum(probs,compliments)
cum_probs <- c(sum_compliments,probs)/maxprobs
cum_probs
