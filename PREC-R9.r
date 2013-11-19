# PREC-R8
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

# We refer to the following papers:

# Castellarin, A., Vogel, R.M., Matalas, N.C. (2005): 
# Probabilistic behaviour of a regional envelope curve.
# Water Resour. Res., 41, W06018, doi:10.1029/2004WR003042.

# Castellarin, A. (2007): 
# Probabilistic envelope curves for design flood estimation at ungauged sites.
# Water Resour. Res., 43(4), W04406, doi:10.1029/2005/WR004384.

# Guse, B., Castellarin, A., Thieken, A.H., Merz, B. (2009):
# Effects of intersite dependence of nested catchment structures on probabilistic regional envelope curves,
# Hydrol. Earth Syst. Sci., 13(9), 1699-1712.

########################################################

# The R file uses the list option named here PREC
# PREC$input_data includes all input data
# PREC$intermed includes intermediate data
# PREC$results includes the results

#set working directory
library(nsRFA)

#setwd("d:\\R\\PREC_R\\functions")

PREC <- list()

## Loading of input parameters

# site data of the region
#region_all <- read.table(paste(getwd(),"\\region_reg_syn.txt", sep=""), header=T)
region_all <- read.table("region_reg_syn.txt", header=T)

# ams matrix
#ams_stat_all <- read.table(paste(getwd(),"\\ams_reg_syn_02.txt", sep=""), header=T, row.names=1)
ams_stat_all <- read.table("ams_reg_syn_02.txt", header=T, row.names=1)

PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all

# statistical values for the region
source("regional_statistic.r")
PREC$result$statistic <- regional_statistic(PREC$input_data)
                     
# homogeneity test
source("hw_region.r")
PREC$result$hw <- hw_region(PREC$input_data)

## further calculation only when hw[1] < thresh 
# distances between sites
source("coordinates.r")
PREC$intermed$distmat <- coordinates(PREC$input_data$region)

# correlation between sites
source("correl.r")
PREC$intermed <- correl(PREC$input_data$ams, PREC$intermed)

#####
source("crosscorr_vectors.r")
PREC$intermed<-crosscorr_vectors(PREC)

# optimation of the TS model parameters
source("ts_model2.r")
PREC$intermed$ts_par=optim(c(1,1),ts_model2)

# crosscorrelation plot
source("crossdist.r")
#jpeg(paste("corrfunc.jpg",sep="/"), width=640, height=480)
#    par(mar=c(4,4,2,1)+2, las=1, mgp=c(4,1,0), cex.main = 2, cex.axis = 1.5, cex.lab = 2)
     #running mean for double-checking
crossdist(PREC$intermed)
#dev.off()

# slope and intercept of PREC
source("REC_parameters.r")
PREC$result$REC_par <- REC_parameters(PREC$result$statistic)

# REC discharge estimation
source("QREC_estimates.r")
PREC$result$statistic <- QREC_estimates(PREC$result)

## Calculation of REC
source("PREC_plot.r")
par(mfrow=c(1,1),mar=c(6,7,4,2), las=1, xaxp=c(0,4,1), yaxp=c(0,5,1), mgp=c(4.5,1,0),
    cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.5)
    
PREC_plot(PREC$result$statistic, PREC$result$REC_par)
#jpeg(filename="REC%d.jpeg", width = 360, height = 360, pointsize= 12,bg="white",res= NA)
     #,restoreConsole=TRUE)
#dev.off()

# Effective number of observation
source("effective_sites.r")
PREC$result$effective_observations <- effective_sites(PREC$input_data$ams, 
PREC$intermed$distmat, PREC$intermed$ts_par$par, PREC$result$hw)

# output of statistical values for the region
#write.table(file=paste(getwd(), "\\region_statistics.txt",sep=""),col.names=T, row.names=T,PREC$result$statistic, sep=";") 

#write.table(file=paste(getwd(), "\\PREC_results.txt",sep=""),col.names=F, row.names=T,PREC$result$effective_observations, sep=";") 

