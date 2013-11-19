# Plot of the relationship between the distance and the empirical crosscorrelation
# including optimised TS-model
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

crossdist <- function (cdresult)

{

corrv <- cdresult$corrv
distav <- cdresult$distav
noverv <- cdresult$noverv

larger4<-which(noverv>4)
corrv <- corrv[larger4]
distav <- distav[larger4]

xall <- cdresult$ts_par$par[1]
xall[2] <- cdresult$ts_par$par[2]
#RWM_coord <- cdresult$dista
#RWM_xcorr <- cdresult$xcorr
RWM_coord <- distav
RWM_xcorr <- corrv

corr_min = floor(min(corrv)*10)/10
dist_min <- min(distav)
dist_min <- 0
dist_max <- max(distav)

# hundred intermediate values
dist_diff <- (dist_max-dist_min)/100

distx <- seq(dist_min, dist_max, dist_diff)

corrfunc = exp(-xall[1]*distx/(1+xall[2]*distx))

# plot distance vs correlation coefficient
        #running mean for double-checking
        plot(RWM_coord, RWM_xcorr, pch=16, lty=1, lwd=3, col="blue",
        xlim=c(0,max(distav)),ylim=c(-1,1),
        main = "Cross-correlation function",
        xlab= "Distance [km]",ylab = "Correlation coefficient [-]",
        xaxs="i", yaxs="i", cex=0.7)
        lines(distx, corrfunc, lty=2, lwd=2, col="red",xaxs="i", yaxs="i", cex=0.5)
        #points(distav, corrv, pch=16, cex=0.5,
        #ylim = c(corr_min,1),
        #col= "blue")

   }