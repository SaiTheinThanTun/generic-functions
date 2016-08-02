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
B <- rep(0:1,10)
C <- rep(0,20)
M <- cbind(A,B,C)

state.trans(origin, des, params, M)
