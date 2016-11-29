#template for runing both ODE and IBM from the same parameters
library(ibmcraftr)
library(deSolve)


#parameters in the global environment
init.pop <- c(S = 300, I = 1)
lambda = 0.0036
maxtime <- 1000

#parameterizing for ODE
times <- seq(0, maxtime, by = 1)
param_ode <- c(lambda = lambda)

#transient values in ODE
transient_ode <- c("", "", "")


#initialization for IBM
pop <- syn_pop(init.pop)

#transient values in IBM
#1.parameter values
transient_ibm_b4 <- c("", "", "")
#2.ODE functions inside IBM
transient_ibm_within <- c("", "", "")

#evaluation of the transient parameter values
eval(parse(text = c(transient_ibm_b4)))
transient_all <- c(transient_ibm_b4, transient_ibm_within)

#model structures
#ODE
rateofchange <- c("dS <- -lambda*S",
                  "dI <- +lambda*S", "")

#IBM
param_ibm <- list(list(1, 2, rate2prob(lambda)))


result <-
  run_state_trans(timesteps = maxtime, param=param_ibm, transient = transient_all, pop)

#ODE function
testfun <- function(t, state, parameters)
{
  with(as.list(c(state, parameters)),
       {
         # define variables
         eval(parse(text = transient_ode))
         
         # rate of change
         eval(parse(text = rateofchange))
         
         # return the rate of change
         list(c(dS, dI))
       })
}

out <-  ode(y = init.pop, times = times, func = testfun,parms = param_ode)

#plot HERE
plot(result[, 1], type = 'l', col = 'blue', main = paste("IBM VS ODE"))
lines(result[, 2], type = 'l', col = 'red')
lines(out[, 2], type = 'l', col = 'blue')
lines(out[, 3], type = 'l', col = 'red')
