# Optimisation of the TS-model
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

crosscorr_vectors <- function (xsprec)

{

region_all <- xsprec$input_data$region

sites_no <- NROW(region_all)

coordmatrix <- xsprec$intermed$distmat
correlmatrix <- xsprec$intermed$correlmat
novermatrix <- xsprec$intermed$novermat
#tspar <- xsprec$input_data$ts_par

# vector with correlation values
corrv = as.vector(correlmatrix)
NOT_NA_index=!is.na(corrv)
corrv = corrv[NOT_NA_index]
# vector with distance values
distav = as.vector(coordmatrix)
distav = distav[NOT_NA_index]
# vector with overlapping values
noverv = as.vector(novermatrix)
noverv = noverv[NOT_NA_index]

RWM_matrix=rbind(distav[order(distav)],corrv[order(distav)],noverv[order(distav)])
# If possible, bin is selected in order to have 300 binned values,
# and a bin larger than 100, 1/20th of the length otherwise
if (length(distav) > 499) RWM_bin=length(distav)-299 else RWM_bin=round(length(distav)/20,1)

# computation of running mean vectors
RWM_length=length(distav)-RWM_bin+1
RWM_coord=rep(0,RWM_length)
RWM_xcorr=rep(0,RWM_length)
for (iRWM in 1:RWM_length)
{
  RWM_indices=iRWM:(iRWM+RWM_bin-1)
  RWM_coord[iRWM]=mean(RWM_matrix[1,RWM_indices])
  RWM_xcorr[iRWM]=sum(RWM_matrix[2,RWM_indices]*RWM_matrix[3,RWM_indices])/sum(RWM_matrix[3,RWM_indices])
}

#xsprec$intermed$ts_par <- tspar
xsprec$intermed$corrv <- corrv
xsprec$intermed$distav <- distav
#xsprec$intermed$dista <- dista
#xsprec$intermed$xcorr <- xcorr
xsprec$intermed$noverv <- noverv   

return(xsprec$intermed)
}