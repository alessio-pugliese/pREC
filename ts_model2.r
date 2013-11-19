# Computes the sum of squared residuals
# between empirical x-correlation values 
# and theoretical values computed according to 
# Tasker and Stedinger [1985] mdoel
  
# input data
# dista: vector of distances
# xcorr: vector of empirical c-correlations
# lambda: vector of 2 parameters lambda1 and lambda2  

ts_model2=function(lambda)
{
  Model=exp(-lambda[1]*PREC$intermed$distav/(1+lambda[2]*PREC$intermed$distav))
  return(sum((Model-PREC$intermed$corrv)^2))  
} 

  