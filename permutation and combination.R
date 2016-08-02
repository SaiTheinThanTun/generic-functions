#permutation and combination

perm = function(n, x) {
  return(factorial(n) / factorial(n-x))
}

comb = function(n, x) {
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}