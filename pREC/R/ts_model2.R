ts_model2 <-
function(lambda)
{
  Model=exp(-lambda[1]*PREC$intermed$distav/(1+lambda[2]*PREC$intermed$distav))
  return(sum((Model-PREC$intermed$corrv)^2))  
}
