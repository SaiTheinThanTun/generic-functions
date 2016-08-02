#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector statechange(int origin, int newstate, NumericVector pat, NumericVector state, NumericVector ran1, NumericVector ran2) {
  NumericMatrix
  int i, n;
  n=state.size();
  //NumericVector z(n);
  
  for(i=0; i<n; i++){
    if(state[i]==0){
      if(ran1[i]<=inf[pat[i]-1])
      {
        state[i]=1;
      }
    }
    else{
      if(ran2[i]<=rec[pat[i]-1])
      {
        state[i]=0;
      }
      
    }
  }
  
  return state;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
inf <- c(.5,.2,.9)
  rec <- c(.1,.1,.1)
  pat <- c(1,1,2,2,3)
  state <- c(1,0,0,1,0)
  ran1 <- c(.6,.6,.6,.6,.6)
  ran2 <- c(.2,.2,.2,.2,.2)
  
  infectedornot(inf, rec, pat, state, ran1, ran2)
  */
