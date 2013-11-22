REC_parameters <-function(regiondata){
  
qmean_area_log <- regiondata[,7]
area_log <- regiondata[,6]
qmax_area_log <- regiondata[,8]

# calculation of the slope
slope <- lm(qmean_area_log~area_log)
b <- slope$coefficients[2]
a_b <- slope$coefficients[1]

# moving up of intercept
intercep <- qmax_area_log - b*area_log
a <- max(intercep)

recpar <- c(a,b,a_b)

return(recpar)
}
