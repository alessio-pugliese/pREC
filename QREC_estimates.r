# Calculation of the REC discharge
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

QREC_estimates <- function(RECres)

{

area <- RECres$statistic[,1]
area_log <- RECres$statistic[,6]
a <- RECres$REC_par[1]
b <- RECres$REC_par[2]

min_area <- min(area)
max_area <- max(area)

 min_Q_I <- min(a+b*min_area)
max_Q_I <- max(a+b*max_area)

pI <-c(10^min_Q_I, 10^max_Q_I)

q_rec_log <- a+b*area_log

q_rec <- 10^(q_rec_log)*area/100

qrecs <- cbind(q_rec_log, q_rec)

colrec<-c("REC discharge log", "REC discharge")

colnames(qrecs)<-colrec

RECres$statistic <- cbind(RECres$statistic, qrecs)

return(RECres$statistic)

}