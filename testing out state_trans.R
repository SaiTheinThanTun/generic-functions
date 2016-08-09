#f <- function(x, y=parse(text=input$f)){eval(y)}
pop2 <- syn_pop(c(19,1,1,0))
lambda <- .5
beta <- .1
A <- 1
B <- c(2,4)
#C <- c('lambda', 'beta')
C <- c(lambda, beta)
alist <- list(A,B,C)
blist <- list(2,3,100)
listall <- list(alist,blist)
#listall

#listall[1]

# for(i in 1:length(listall)){
#   state_trans(listall[[i]][[1]], listall[[i]][[2]], eval(parse(text=listall[[i]][[3]])), pop2)
# 
# }
Matrix.List <- list()
for(i in 1:length(listall)){
  Matrix.List[[i]] <- state_trans(listall[[i]][[1]], listall[[i]][[2]], listall[[i]][[3]], pop2)
}
Reduce('+', Matrix.List)



#transform text into variable name
eval(parse(text=listall[[2]][[1]]))
class(listall[[2]][[1]])
length(listall[[2]][[1]])

para <- NA
for(i in 1:length(listall[[2]][[1]])){
  para <- append(para, eval(parse(text=listall[[2]][[1]][i])))
}

a <- 3
b <- 2
c <- 5
result <- NA
formulas <- c("a+b+c","(a*b)+c","(a+b)/c")
for(i in 1:length(formulas)){
  result[i] <- eval(parse(text=formulas[i]))
}
